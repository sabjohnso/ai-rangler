#lang racket/base

;; gui-presenter — dispatches presenter commands to widget methods.
;; Depends only on racket/class (for send) and presenter.rkt (for
;; gen:presenter + cmd:* structs). Zero GUI dependencies, so it
;; is testable with plain mock objects.

(require racket/class
         "../presenter.rkt")

(provide gui-presenter
         gui-presenter-chat
         gui-presenter-status
         gui-presenter-input)

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
       [(cmd:begin-code-block? cmd)
        (send chat begin-code-block (cmd:begin-code-block-language cmd))]
       [(cmd:append-code-text? cmd)
        (send chat append-code-text (cmd:append-code-text-text cmd))]
       [(cmd:end-code-block? cmd)
        (send chat end-code-block)]
       [else (void)]))])
