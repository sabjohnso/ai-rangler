#lang racket/base

;; Session tab — composes chat-view, input-area, and status-bar.
;; Delegates event→command mapping to the controller; a local
;; gui-presenter dispatches commands to the appropriate widgets.

(require racket/class
         racket/gui/base
         "../session.rkt"
         "../controller.rkt"
         "gui-presenter.rkt"
         "chat-view.rkt"
         "input-area.rkt"
         "status-bar.rkt")

(provide session-tab%)

;; ─── Session tab ────────────────────────────────────────────────────────────

(define session-tab%
  (class vertical-panel%
    (init-field session)
    (super-new)

    ;; ─── Child widgets ─────────────────────────────────────────────────

    (define input (new input-area%
                       [parent this]
                       [on-send (λ (msg) (controller-send! ctrl msg))]))

    (define chat (new chat-view% [parent this]))

    (define status (new status-bar% [parent this]))

    ;; ─── Controller + presenter wiring ─────────────────────────────────

    (define presenter (gui-presenter chat status input))
    (define ctrl (make-controller session presenter))

    ;; ─── Event drain timer ─────────────────────────────────────────────

    (define timer
      (new timer%
           [notify-callback (λ () (controller-drain! ctrl))]
           [interval 50]))

    ;; ─── Theme ─────────────────────────────────────────────────────────

    (define/public (apply-theme)
      (send chat apply-theme)
      (send input apply-theme)
      (send status apply-theme))

    ;; ─── Cleanup ───────────────────────────────────────────────────────

    (define/public (stop)
      (send timer stop)
      (session-stop session))))
