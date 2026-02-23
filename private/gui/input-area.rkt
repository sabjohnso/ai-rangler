#lang racket/base

;; Input area — text field + send button for user messages.
;; Calls the on-send callback when the user presses Enter or clicks Send.
;; Can be enabled/disabled based on session state.

(require racket/class
         racket/gui/base
         "theme.rkt")

(provide input-area%)

(define input-area%
  (class horizontal-panel%
    (init-field [on-send (λ (msg) (void))])
    (super-new [stretchable-height #f])

    (define text-field
      (new text-field%
           [parent this]
           [label ""]
           [callback (λ (tf evt)
                       (when (equal? (send evt get-event-type) 'text-field-enter)
                         (do-send)))]))

    ;; Match font, foreground and background to the chat view.
    (define (apply-input-style!)
      (define t (current-theme))
      (define ed (send text-field get-editor))
      (define sd (new style-delta%))
      (send sd set-family 'modern)
      (send sd set-delta-foreground (parse-color (theme-assistant-color t)))
      (send ed change-style sd 0 (send ed last-position))
      (send text-field set-field-background
            (parse-color (theme-background t))))

    (apply-input-style!)

    (define send-button
      (new button%
           [parent this]
           [label "Send"]
           [callback (λ (btn evt) (do-send))]))

    (define (do-send)
      (define msg (send text-field get-value))
      (when (positive? (string-length msg))
        (on-send msg)
        (send text-field set-value "")))

    (define/public (set-enabled enabled?)
      (send text-field enable enabled?)
      (send send-button enable enabled?))

    (define/public (apply-theme)
      (apply-input-style!))

    (define/public (focus-input)
      (send text-field focus))))
