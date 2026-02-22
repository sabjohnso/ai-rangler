#lang racket/base

;; Tests for gui-presenter dispatch — uses mock widget classes
;; (plain object%, no GUI dependencies) to verify that each
;; cmd:* struct dispatches to the correct widget method.

(require rackunit
         racket/class
         "../private/presenter.rkt"
         "../private/gui/gui-presenter.rkt")

;; ─── Mock widgets ──────────────────────────────────────────────────────────

(define (make-recorder)
  (box '()))

(define (record! recorder method-name . args)
  (set-box! recorder
            (append (unbox recorder)
                    (list (cons method-name args)))))

(define mock-chat%
  (class object%
    (super-new)
    (define calls (make-recorder))
    (define/public (get-calls)  (unbox calls))
    (define/public (reset!)     (set-box! calls '()))
    (define/public (append-user-message text)
      (record! calls 'append-user-message text))
    (define/public (begin-assistant-message)
      (record! calls 'begin-assistant-message))
    (define/public (append-assistant-text text)
      (record! calls 'append-assistant-text text))
    (define/public (append-tool-notification tool-name action)
      (record! calls 'append-tool-notification tool-name action))
    (define/public (append-error message)
      (record! calls 'append-error message))
    (define/public (append-result text cost-usd)
      (record! calls 'append-result text cost-usd))))

(define mock-status%
  (class object%
    (super-new)
    (define calls (make-recorder))
    (define/public (get-calls)  (unbox calls))
    (define/public (reset!)     (set-box! calls '()))
    (define/public (set-state state)
      (record! calls 'set-state state))
    (define/public (set-tool-name name)
      (record! calls 'set-tool-name name))
    (define/public (set-cost cost-usd)
      (record! calls 'set-cost cost-usd))))

(define mock-input%
  (class object%
    (super-new)
    (define calls (make-recorder))
    (define/public (get-calls)  (unbox calls))
    (define/public (reset!)     (set-box! calls '()))
    (define/public (set-enabled enabled?)
      (record! calls 'set-enabled enabled?))))

;; ─── Test fixtures ─────────────────────────────────────────────────────────

(define chat   (new mock-chat%))
(define status (new mock-status%))
(define input  (new mock-input%))
(define gp     (gui-presenter chat status input))

(define (reset-all!)
  (send chat   reset!)
  (send status reset!)
  (send input  reset!))

;; ─── Tests ─────────────────────────────────────────────────────────────────

(module+ test

  (test-case "presenter? recognises gui-presenter"
    (check-true (presenter? gp)))

  (test-case "cmd:show-user-message → chat append-user-message"
    (reset-all!)
    (present! gp (cmd:show-user-message "hello"))
    (check-equal? (send chat get-calls)
                  '((append-user-message . ("hello"))))
    (check-equal? (send status get-calls) '())
    (check-equal? (send input  get-calls) '()))

  (test-case "cmd:begin-assistant-message → chat begin-assistant-message"
    (reset-all!)
    (present! gp (cmd:begin-assistant-message))
    (check-equal? (send chat get-calls)
                  '((begin-assistant-message))))

  (test-case "cmd:append-assistant-text → chat append-assistant-text"
    (reset-all!)
    (present! gp (cmd:append-assistant-text "chunk"))
    (check-equal? (send chat get-calls)
                  '((append-assistant-text . ("chunk")))))

  (test-case "cmd:show-tool-notification → chat append-tool-notification"
    (reset-all!)
    (present! gp (cmd:show-tool-notification "Read" "reading file"))
    (check-equal? (send chat get-calls)
                  '((append-tool-notification . ("Read" "reading file")))))

  (test-case "cmd:show-error → chat append-error"
    (reset-all!)
    (present! gp (cmd:show-error "something broke"))
    (check-equal? (send chat get-calls)
                  '((append-error . ("something broke")))))

  (test-case "cmd:show-result → chat append-result"
    (reset-all!)
    (present! gp (cmd:show-result "done" 0.05))
    (check-equal? (send chat get-calls)
                  '((append-result . ("done" 0.05)))))

  (test-case "cmd:set-state → status set-state"
    (reset-all!)
    (present! gp (cmd:set-state 'thinking))
    (check-equal? (send status get-calls)
                  '((set-state . (thinking))))
    (check-equal? (send chat  get-calls) '())
    (check-equal? (send input get-calls) '()))

  (test-case "cmd:set-tool-name → status set-tool-name"
    (reset-all!)
    (present! gp (cmd:set-tool-name "Bash"))
    (check-equal? (send status get-calls)
                  '((set-tool-name . ("Bash")))))

  (test-case "cmd:set-cost → status set-cost"
    (reset-all!)
    (present! gp (cmd:set-cost 1.23))
    (check-equal? (send status get-calls)
                  '((set-cost . (1.23)))))

  (test-case "cmd:set-input-enabled → input set-enabled"
    (reset-all!)
    (present! gp (cmd:set-input-enabled #t))
    (check-equal? (send input get-calls)
                  '((set-enabled . (#t))))
    (check-equal? (send chat   get-calls) '())
    (check-equal? (send status get-calls) '()))

  (test-case "unknown command → no calls on any mock"
    (reset-all!)
    (present! gp "not-a-command")
    (check-equal? (send chat   get-calls) '())
    (check-equal? (send status get-calls) '())
    (check-equal? (send input  get-calls) '())))
