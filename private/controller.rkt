#lang racket/base

;; Controller — maps session events to presentation commands.
;; Pure bridge between gen:session and gen:presenter.
;; No GUI imports; any presenter implementation works.

(require racket/async-channel
         "session.rkt"
         "event.rkt"
         "presenter.rkt")

(provide make-controller
         controller-drain!
         controller-send!)

;; ─── Controller struct ──────────────────────────────────────────────────────

(struct controller (session presenter events-ch in-turn-box)
  #:transparent)

(define (make-controller session presenter)
  (controller session
              presenter
              (session-events session)
              (box #f)))

;; ─── Send a user message ────────────────────────────────────────────────────

(define (controller-send! ctrl msg)
  (present! (controller-presenter ctrl) (cmd:show-user-message msg))
  (present! (controller-presenter ctrl) (cmd:set-input-enabled #f))
  (session-send (controller-session ctrl) msg))

;; ─── Drain all pending events ───────────────────────────────────────────────

(define (controller-drain! ctrl)
  (let loop ()
    (define evt (async-channel-try-get (controller-events-ch ctrl)))
    (when evt
      (dispatch-event ctrl evt)
      (loop))))

;; ─── Event dispatch ─────────────────────────────────────────────────────────

(define (dispatch-event ctrl evt)
  (define p (controller-presenter ctrl))
  (define in-turn? (unbox (controller-in-turn-box ctrl)))

  (cond
    [(event:init? evt)
     (present! p (cmd:set-state 'idle))]

    [(event:text-delta? evt)
     (unless in-turn?
       (present! p (cmd:begin-assistant-message))
       (set-box! (controller-in-turn-box ctrl) #t))
     (present! p (cmd:append-assistant-text (event:text-delta-text evt)))
     (present! p (cmd:set-state 'working))]

    [(event:tool-start? evt)
     (present! p (cmd:show-tool-notification
                  (event:tool-start-tool-name evt) "started"))
     (present! p (cmd:set-state 'tool-active))
     (present! p (cmd:set-tool-name (event:tool-start-tool-name evt)))]

    [(event:tool-end? evt)
     (present! p (cmd:set-state 'working))
     (present! p (cmd:set-tool-name #f))]

    [(event:assistant-message? evt)
     (unless in-turn?
       (present! p (cmd:begin-assistant-message))
       (for ([block (event:assistant-message-content evt)])
         (when (and (hash? block)
                    (equal? (hash-ref block 'type #f) "text"))
           (present! p (cmd:append-assistant-text
                        (hash-ref block 'text ""))))))
     (set-box! (controller-in-turn-box ctrl) #f)
     (present! p (cmd:set-state 'idle))
     (present! p (cmd:set-input-enabled #t))]

    [(event:result? evt)
     (present! p (cmd:show-result (event:result-text evt)
                                  (event:result-cost-usd evt)))
     (present! p (cmd:set-state 'idle))
     (when (event:result-cost-usd evt)
       (present! p (cmd:set-cost (event:result-cost-usd evt))))
     (present! p (cmd:set-input-enabled #t))
     (set-box! (controller-in-turn-box ctrl) #f)]

    [(event:error? evt)
     (present! p (cmd:show-error (event:error-message evt)))
     (present! p (cmd:set-state 'error))
     (present! p (cmd:set-input-enabled #t))
     (set-box! (controller-in-turn-box ctrl) #f)]

    [else (void)]))
