#lang racket/base

(require rackunit
         racket/list
         racket/async-channel
         "../private/session.rkt"
         "../private/event.rkt"
         "../private/presenter.rkt"
         "../private/controller.rkt")

;; ─── Test helpers ───────────────────────────────────────────────────────────

;; list-presenter: collects commands in a box for assertions.
(struct list-presenter (commands-box)
  #:methods gen:presenter
  [(define (present! self cmd)
     (set-box! (list-presenter-commands-box self)
               (append (unbox (list-presenter-commands-box self))
                       (list cmd))))])

(define (make-lp)
  (list-presenter (box '())))

(define (lp-commands lp)
  (unbox (list-presenter-commands-box lp)))

(define (lp-reset! lp)
  (set-box! (list-presenter-commands-box lp) '()))

;; mock-session: records sent messages, provides async-channel for events.
(struct mock-session (id state-box events-ch sent-box)
  #:methods gen:session
  [(define (session-send s msg)
     (set-box! (mock-session-sent-box s)
               (append (unbox (mock-session-sent-box s)) (list msg))))
   (define (session-state s) (unbox (mock-session-state-box s)))
   (define (session-events s) (mock-session-events-ch s))
   (define (session-stop s) (set-box! (mock-session-state-box s) 'stopped))
   (define (session-id s) (mock-session-id s))
   (define (session-info s) (hasheq 'type "mock"))])

(define (make-mock)
  (mock-session "test-1" (box 'idle) (make-async-channel) (box '())))

(define (mock-sent ms)
  (unbox (mock-session-sent-box ms)))

;; Push an event into the mock session's channel and drain via controller.
(define (push-and-drain! ctrl ms evt)
  (async-channel-put (mock-session-events-ch ms) evt)
  (controller-drain! ctrl))

;; ─── controller-send! ───────────────────────────────────────────────────────

(test-case "controller-send! emits show-user-message and disables input"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (controller-send! ctrl "hello world")
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 2)
  (check-pred cmd:show-user-message? (first cmds))
  (check-equal? (cmd:show-user-message-text (first cmds)) "hello world")
  (check-pred cmd:set-input-enabled? (second cmds))
  (check-false (cmd:set-input-enabled-enabled? (second cmds)))
  ;; Also forwards message to session
  (check-equal? (mock-sent ms) '("hello world")))

;; ─── event:init ─────────────────────────────────────────────────────────────

(test-case "event:init → set-state idle"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms (event:init 1000 "test-1" "test-1" '("Bash") "opus"))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 1)
  (check-pred cmd:set-state? (first cmds))
  (check-equal? (cmd:set-state-state (first cmds)) 'idle))

;; ─── event:text-delta (first) ───────────────────────────────────────────────

(test-case "first text-delta → begin-assistant-message + append-text + set-state working"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  ;; Text ending with \n is emitted immediately; partial lines are buffered.
  (push-and-drain! ctrl ms (event:text-delta 2000 "test-1" "Hello\n" 0))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 3)
  (check-pred cmd:begin-assistant-message? (first cmds))
  (check-pred cmd:append-assistant-text? (second cmds))
  (check-equal? (cmd:append-assistant-text-text (second cmds)) "Hello\n")
  (check-pred cmd:set-state? (third cmds))
  (check-equal? (cmd:set-state-state (third cmds)) 'working))

;; ─── event:text-delta (second — no duplicate separator) ─────────────────────

(test-case "second text-delta → no duplicate begin-assistant-message"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms (event:text-delta 2000 "test-1" "Hello\n" 0))
  (lp-reset! lp)
  (push-and-drain! ctrl ms (event:text-delta 2001 "test-1" " world\n" 0))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 2)
  (check-pred cmd:append-assistant-text? (first cmds))
  (check-equal? (cmd:append-assistant-text-text (first cmds)) " world\n")
  (check-pred cmd:set-state? (second cmds))
  (check-equal? (cmd:set-state-state (second cmds)) 'working))

;; ─── event:tool-start ───────────────────────────────────────────────────────

(test-case "event:tool-start → tool notification + state + tool name"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms
    (event:tool-start 3000 "test-1" "toolu_01" "Read" (hasheq 'file "/tmp")))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 3)
  (check-pred cmd:show-tool-notification? (first cmds))
  (check-equal? (cmd:show-tool-notification-tool-name (first cmds)) "Read")
  (check-equal? (cmd:show-tool-notification-action (first cmds)) "started")
  (check-pred cmd:set-state? (second cmds))
  (check-equal? (cmd:set-state-state (second cmds)) 'tool-active)
  (check-pred cmd:set-tool-name? (third cmds))
  (check-equal? (cmd:set-tool-name-name (third cmds)) "Read"))

;; ─── event:tool-end ─────────────────────────────────────────────────────────

(test-case "event:tool-end → state working + tool name cleared"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms (event:tool-end 3100 "test-1" "toolu_01"))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 2)
  (check-pred cmd:set-state? (first cmds))
  (check-equal? (cmd:set-state-state (first cmds)) 'working)
  (check-pred cmd:set-tool-name? (second cmds))
  (check-false (cmd:set-tool-name-name (second cmds))))

;; ─── event:assistant-message (no prior deltas) ─────────────────────────────

(test-case "assistant-message without prior deltas → shows content blocks"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (define blocks (list (hasheq 'type "text" 'text "Hi there")))
  (push-and-drain! ctrl ms (event:assistant-message 4000 "test-1" blocks "opus"))
  (define cmds (lp-commands lp))
  ;; begin-assistant-message, append-assistant-text "Hi there",
  ;; set-state idle, set-input-enabled #t
  (check-equal? (length cmds) 4)
  (check-pred cmd:begin-assistant-message? (first cmds))
  (check-pred cmd:append-assistant-text? (second cmds))
  (check-equal? (cmd:append-assistant-text-text (second cmds)) "Hi there")
  (check-pred cmd:set-state? (third cmds))
  (check-equal? (cmd:set-state-state (third cmds)) 'idle)
  (check-pred cmd:set-input-enabled? (fourth cmds))
  (check-true (cmd:set-input-enabled-enabled? (fourth cmds))))

;; ─── event:assistant-message (after deltas) ─────────────────────────────────

(test-case "assistant-message after deltas → skips content, resets turn"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  ;; First delta opens the turn.
  (push-and-drain! ctrl ms (event:text-delta 2000 "test-1" "streamed\n" 0))
  (lp-reset! lp)
  ;; assistant-message arrives after deltas.
  (define blocks (list (hasheq 'type "text" 'text "streamed")))
  (push-and-drain! ctrl ms (event:assistant-message 4000 "test-1" blocks "opus"))
  (define cmds (lp-commands lp))
  ;; set-state idle, set-input-enabled #t — no begin/append
  (check-equal? (length cmds) 2)
  (check-pred cmd:set-state? (first cmds))
  (check-equal? (cmd:set-state-state (first cmds)) 'idle)
  (check-pred cmd:set-input-enabled? (second cmds))
  (check-true (cmd:set-input-enabled-enabled? (second cmds))))

;; ─── event:assistant-message (after deltas, pending fence buffer) ─────────

(test-case "assistant-message after deltas flushes pending fence buffer"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  ;; Delta with no trailing newline — text is buffered by fence-track.
  (push-and-drain! ctrl ms (event:text-delta 2000 "test-1" "partial text" 0))
  (lp-reset! lp)
  ;; Assistant-message should flush the buffered text before resetting.
  (define blocks (list (hasheq 'type "text" 'text "partial text")))
  (push-and-drain! ctrl ms (event:assistant-message 4000 "test-1" blocks "opus"))
  (define cmds (lp-commands lp))
  ;; Should have: append-assistant-text "partial text" (flushed),
  ;; set-state idle, set-input-enabled #t
  (check-equal? (length cmds) 3)
  (check-pred cmd:append-assistant-text? (first cmds))
  (check-equal? (cmd:append-assistant-text-text (first cmds)) "partial text")
  (check-pred cmd:set-state? (second cmds))
  (check-equal? (cmd:set-state-state (second cmds)) 'idle)
  (check-pred cmd:set-input-enabled? (third cmds))
  (check-true (cmd:set-input-enabled-enabled? (third cmds))))

;; ─── event:result ───────────────────────────────────────────────────────────

(test-case "event:result → show-result + set-state idle + set-cost + enable input"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms
    (event:result 9000 "test-1" #f "Done." 0.015
                  (hasheq 'input_tokens 100 'output_tokens 50) 5000 2))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 4)
  (check-pred cmd:show-result? (first cmds))
  (check-equal? (cmd:show-result-text (first cmds)) "Done.")
  (check-equal? (cmd:show-result-cost-usd (first cmds)) 0.015)
  (check-pred cmd:set-state? (second cmds))
  (check-equal? (cmd:set-state-state (second cmds)) 'idle)
  (check-pred cmd:set-cost? (third cmds))
  (check-equal? (cmd:set-cost-cost-usd (third cmds)) 0.015)
  (check-pred cmd:set-input-enabled? (fourth cmds))
  (check-true (cmd:set-input-enabled-enabled? (fourth cmds))))

;; ─── event:error ────────────────────────────────────────────────────────────

(test-case "event:error → show-error + set-state error + enable input"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms (event:error 9500 "test-1" "something broke"))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 3)
  (check-pred cmd:show-error? (first cmds))
  (check-equal? (cmd:show-error-message (first cmds)) "something broke")
  (check-pred cmd:set-state? (second cmds))
  (check-equal? (cmd:set-state-state (second cmds)) 'error)
  (check-pred cmd:set-input-enabled? (third cmds))
  (check-true (cmd:set-input-enabled-enabled? (third cmds))))

;; ─── controller-drain! processes all queued events ──────────────────────────

(test-case "controller-drain! processes all queued events in one call"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (define ch (mock-session-events-ch ms))
  ;; Queue up 3 events before draining.
  (async-channel-put ch (event:init 1000 "test-1" "test-1" '() "m"))
  (async-channel-put ch (event:text-delta 2000 "test-1" "Hi\n" 0))
  (async-channel-put ch (event:tool-start 3000 "test-1" "t1" "Bash" (hasheq)))
  (controller-drain! ctrl)
  (define cmds (lp-commands lp))
  ;; init→1, first delta→3, tool-start→3 = 7 commands
  (check-equal? (length cmds) 7))

;; ─── Code fence handling ──────────────────────────────────────────────────────

(test-case "text-delta with code fence emits begin/end-code-block commands"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  ;; A delta containing a complete fenced code block
  (push-and-drain! ctrl ms
    (event:text-delta 2000 "test-1"
                      "before\n```racket\n(+ 1 2)\n```\nafter\n" 0))
  (define cmds (lp-commands lp))
  ;; begin-assistant-message (first delta)
  ;; cmd:append-assistant-text "before\n" (prose)
  ;; cmd:begin-code-block "racket"
  ;; cmd:append-code-text "(+ 1 2)\n"
  ;; cmd:end-code-block
  ;; cmd:append-assistant-text "after\n" (prose)
  ;; cmd:set-state 'working
  (check-pred cmd:begin-assistant-message? (first cmds))
  (check-pred cmd:append-assistant-text? (second cmds))
  (check-equal? (cmd:append-assistant-text-text (second cmds)) "before\n")
  (check-pred cmd:begin-code-block? (third cmds))
  (check-equal? (cmd:begin-code-block-language (third cmds)) "racket")
  (check-pred cmd:append-code-text? (fourth cmds))
  (check-equal? (cmd:append-code-text-text (fourth cmds)) "(+ 1 2)\n")
  (check-pred cmd:end-code-block? (fifth cmds))
  (check-pred cmd:append-assistant-text? (sixth cmds))
  (check-equal? (cmd:append-assistant-text-text (sixth cmds)) "after\n")
  (check-pred cmd:set-state? (seventh cmds)))

(test-case "text-delta with prose only emits append-assistant-text"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  (push-and-drain! ctrl ms
    (event:text-delta 2000 "test-1" "just plain text\n" 0))
  (define cmds (lp-commands lp))
  ;; begin-assistant-message, append-assistant-text, set-state working
  (check-equal? (length cmds) 3)
  (check-pred cmd:begin-assistant-message? (first cmds))
  (check-pred cmd:append-assistant-text? (second cmds))
  (check-equal? (cmd:append-assistant-text-text (second cmds)) "just plain text\n")
  (check-pred cmd:set-state? (third cmds)))

(test-case "fence state resets at turn end"
  (define ms (make-mock))
  (define lp (make-lp))
  (define ctrl (make-controller ms lp))
  ;; Start a code block but don't close it
  (push-and-drain! ctrl ms
    (event:text-delta 2000 "test-1" "```racket\ncode" 0))
  (lp-reset! lp)
  ;; Turn end should flush pending code
  (push-and-drain! ctrl ms
    (event:result 9000 "test-1" #f "Done." 0.01
                  (hasheq 'input_tokens 10 'output_tokens 5) 1000 1))
  (define cmds (lp-commands lp))
  ;; Should have: append-code-text "code" (flushed), then result commands
  (check-pred cmd:append-code-text? (first cmds))
  (check-equal? (cmd:append-code-text-text (first cmds)) "code"))
