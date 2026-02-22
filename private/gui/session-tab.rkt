#lang racket/base

;; Session tab — composes chat-view, input-area, and status-bar.
;; Delegates event→command mapping to the controller; a local
;; gui-presenter dispatches commands to the appropriate widgets.

(require racket/class
         racket/gui/base
         "../session.rkt"
         "../presenter.rkt"
         "../controller.rkt"
         "chat-view.rkt"
         "input-area.rkt"
         "status-bar.rkt")

(provide session-tab%)

;; ─── GUI presenter ──────────────────────────────────────────────────────────

(struct gui-presenter (chat status input)
  #:methods gen:presenter
  [(define (present! self cmd)
     (define chat   (gui-presenter-chat self))
     (define status (gui-presenter-status self))
     (define input  (gui-presenter-input self))
     (cond
       [(cmd:show-user-message? cmd)
        (send chat append-user-message (cmd:show-user-message-text cmd))]
       [(cmd:begin-assistant-message? cmd)
        (send chat begin-assistant-message)]
       [(cmd:append-assistant-text? cmd)
        (send chat append-assistant-text (cmd:append-assistant-text-text cmd))]
       [(cmd:show-tool-notification? cmd)
        (send chat append-tool-notification
              (cmd:show-tool-notification-tool-name cmd)
              (cmd:show-tool-notification-action cmd))]
       [(cmd:show-error? cmd)
        (send chat append-error (cmd:show-error-message cmd))]
       [(cmd:show-result? cmd)
        (send chat append-result
              (cmd:show-result-text cmd)
              (cmd:show-result-cost-usd cmd))]
       [(cmd:set-state? cmd)
        (send status set-state (cmd:set-state-state cmd))]
       [(cmd:set-tool-name? cmd)
        (send status set-tool-name (cmd:set-tool-name-name cmd))]
       [(cmd:set-cost? cmd)
        (send status set-cost (cmd:set-cost-cost-usd cmd))]
       [(cmd:set-input-enabled? cmd)
        (send input set-enabled (cmd:set-input-enabled-enabled? cmd))]
       [else (void)]))])

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
      (send input apply-theme))

    ;; ─── Cleanup ───────────────────────────────────────────────────────

    (define/public (stop)
      (send timer stop)
      (session-stop session))))
