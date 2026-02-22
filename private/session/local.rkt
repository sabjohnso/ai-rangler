#lang racket/base

;; Local session — manages a claude-code subprocess, reads its NDJSON
;; output, translates to typed events, and exposes the gen:session
;; interface. A reader thread handles I/O; events flow through an
;; async-channel to the consumer (typically the GUI timer).

(require racket/async-channel
         racket/port
         racket/string
         json
         "../session.rkt"
         "../event.rkt"
         "../ndjson.rkt"
         "../json-event.rkt"
         "../process.rkt"
         "../stderr-ring.rkt")

(provide make-local-session)

;; ─── Local session struct ────────────────────────────────────────────────────

(struct local-session (id state-box events-ch process-box
                       reader-thread-box stopping?-box
                       stderr-ring)
  #:methods gen:session
  [(define (session-send s msg)
     (local-session-do-send s msg))
   (define (session-state s)
     (unbox (local-session-state-box s)))
   (define (session-events s)
     (local-session-events-ch s))
   (define (session-stop s)
     (local-session-do-stop s))
   (define (session-id s)
     (local-session-id s))
   (define (session-info s)
     (hasheq 'type "local"
             'session-id (local-session-id s)))])

;; ─── Constructor ─────────────────────────────────────────────────────────────

(define (make-local-session #:command [command #f]
                            #:session-id [sid (gensym "local-")]
                            #:cwd [cwd #f]
                            #:claude-path [claude-path #f]
                            #:stderr-buffer-lines [stderr-buf-lines 50])
  (define state-box (box 'init))
  (define events-ch (make-async-channel))
  (define cp
    (cond
      [command (make-claude-process #:command command)]
      [claude-path
       (make-claude-process #:command (make-claude-command #:claude-path claude-path)
                            #:cwd cwd)]
      [else (make-claude-process #:cwd cwd)]))
  (define process-box (box cp))
  (define reader-thread-box (box #f))
  (define stopping?-box (box #f))
  (define ring (make-stderr-ring #:max-lines stderr-buf-lines))

  (define s (local-session (if (symbol? sid) (symbol->string sid) sid)
                           state-box events-ch process-box
                           reader-thread-box stopping?-box
                           ring))

  ;; Start the reader thread
  (set-box! reader-thread-box (start-reader s))
  s)

;; ─── Reader thread ───────────────────────────────────────────────────────────

;; The reader thread reads NDJSON from the subprocess stdout,
;; translates each line to a session event, updates state, and
;; pushes events to the async-channel. On unexpected process exit,
;; reads stderr and pushes an error event so the GUI can report it.
(define (start-reader s)
  (define cp (unbox (local-session-process-box s)))
  (define sid (local-session-id s))
  (define state-box (local-session-state-box s))
  (define events-ch (local-session-events-ch s))
  (define stopping?-box (local-session-stopping?-box s))
  (define ring (local-session-stderr-ring s))
  (define stdout (claude-process-stdout cp))
  (define stderr (claude-process-stderr cp))

  ;; Drain stderr continuously so the pipe buffer never fills
  (start-stderr-drain ring stderr)

  (thread
   (λ ()
     (let loop ()
       (define v (read-ndjson-line stdout))
       (cond
         [(eof-object? v)
          ;; If stop was requested, just transition quietly.
          (cond
            [(unbox stopping?-box)
             (set-box! state-box 'stopped)]
            [else
             ;; Unexpected exit — use buffered stderr lines for diagnostics
             (define err-lines (stderr-ring-lines ring))
             (define err-text (string-join err-lines "\n"))
             (define exit-code (claude-process-wait cp))
             (define has-error?
               (or (not (equal? exit-code 0))
                   (positive? (string-length err-text))))
             (cond
               [has-error?
                (define msg
                  (if (positive? (string-length err-text))
                      (format "Process exited (~a): ~a" exit-code err-text)
                      (format "Process exited with code ~a" exit-code)))
                (define evt (event:error (current-inexact-milliseconds) sid msg))
                (update-state! state-box evt)
                (async-channel-put events-ch evt)]
               [else
                (set-box! state-box 'stopped)])])]
         [(not v)
          ;; Malformed line — skip
          (loop)]
         [else
          (define evt (json->session-event v sid))
          ;; Update state based on event type
          (update-state! state-box evt)
          ;; Push to channel
          (async-channel-put events-ch evt)
          (loop)])))))

;; ─── State machine ───────────────────────────────────────────────────────────

;; States: init → idle ↔ working ↔ tool-active; error/stopped are terminal.
(define (update-state! state-box evt)
  (cond
    [(event:init? evt)
     (set-box! state-box 'idle)]
    [(event:assistant-message? evt)
     (set-box! state-box 'idle)]
    [(event:text-delta? evt)
     (set-box! state-box 'working)]
    [(event:tool-start? evt)
     (set-box! state-box 'tool-active)]
    [(event:tool-end? evt)
     (set-box! state-box 'working)]
    [(event:result? evt)
     (if (event:result-is-error? evt)
         (set-box! state-box 'error)
         (set-box! state-box 'idle))]
    [(event:error? evt)
     (set-box! state-box 'error)]
    [else (void)]))

;; ─── Send ────────────────────────────────────────────────────────────────────

(define (local-session-do-send s msg)
  (define cp (unbox (local-session-process-box s)))
  (when (claude-process-running? cp)
    (write-ndjson-line (hasheq 'type "user"
                               'message (hasheq 'role "user"
                                                 'content msg))
                       (claude-process-stdin cp))))

;; ─── Stop ────────────────────────────────────────────────────────────────────

(define (local-session-do-stop s)
  (define state-box (local-session-state-box s))
  (define stopping?-box (local-session-stopping?-box s))
  (define cp (unbox (local-session-process-box s)))
  ;; Signal the reader thread that this is intentional
  (set-box! stopping?-box #t)
  (when (claude-process-running? cp)
    (claude-process-kill cp))
  (set-box! state-box 'stopped))
