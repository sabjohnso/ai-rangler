#lang racket/base

(require rackunit
         "../private/presenter.rkt")

;; ─── list-presenter: a mock that collects commands in a box ─────────────────

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

;; ─── Contract: gen:presenter ────────────────────────────────────────────────

(test-case "list-presenter satisfies presenter?"
  (check-pred presenter? (make-lp)))

(test-case "present! records a single command"
  (define lp (make-lp))
  (present! lp (cmd:show-error "oops"))
  (check-equal? (length (lp-commands lp)) 1)
  (check-pred cmd:show-error? (car (lp-commands lp)))
  (check-equal? (cmd:show-error-message (car (lp-commands lp))) "oops"))

(test-case "present! accumulates multiple commands in order"
  (define lp (make-lp))
  (present! lp (cmd:set-state 'idle))
  (present! lp (cmd:show-user-message "hi"))
  (present! lp (cmd:set-input-enabled #f))
  (define cmds (lp-commands lp))
  (check-equal? (length cmds) 3)
  (check-pred cmd:set-state? (list-ref cmds 0))
  (check-pred cmd:show-user-message? (list-ref cmds 1))
  (check-pred cmd:set-input-enabled? (list-ref cmds 2)))

;; ─── Command struct construction and field access ───────────────────────────

(test-case "all 10 command structs are constructable with correct fields"
  (define c1 (cmd:show-user-message "hello"))
  (check-equal? (cmd:show-user-message-text c1) "hello")

  (define c2 (cmd:begin-assistant-message))
  (check-pred cmd:begin-assistant-message? c2)

  (define c3 (cmd:append-assistant-text "world"))
  (check-equal? (cmd:append-assistant-text-text c3) "world")

  (define c4 (cmd:show-tool-notification "Read" "started"))
  (check-equal? (cmd:show-tool-notification-tool-name c4) "Read")
  (check-equal? (cmd:show-tool-notification-action c4) "started")

  (define c5 (cmd:show-error "bad"))
  (check-equal? (cmd:show-error-message c5) "bad")

  (define c6 (cmd:show-result "Done" 0.05))
  (check-equal? (cmd:show-result-text c6) "Done")
  (check-equal? (cmd:show-result-cost-usd c6) 0.05)

  (define c7 (cmd:set-state 'working))
  (check-equal? (cmd:set-state-state c7) 'working)

  (define c8 (cmd:set-tool-name "Bash"))
  (check-equal? (cmd:set-tool-name-name c8) "Bash")

  (define c9 (cmd:set-cost 0.12))
  (check-equal? (cmd:set-cost-cost-usd c9) 0.12)

  (define c10 (cmd:set-input-enabled #t))
  (check-true (cmd:set-input-enabled-enabled? c10)))
