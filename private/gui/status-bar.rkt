#lang racket/base

;; Status bar — displays session state, active tool name, and cost.
;; Updated by the session-tab when events arrive.

(require racket/class
         racket/gui/base
         "theme.rkt")

(provide status-bar%)

(define status-bar%
  (class horizontal-panel%
    (super-new [stretchable-height #f]
               [alignment '(left center)])

    (define state-label
      (new message%
           [parent this]
           [label "State: init"]
           [auto-resize #t]))

    (define tool-label
      (new message%
           [parent this]
           [label ""]
           [auto-resize #t]))

    (define cost-label
      (new message%
           [parent this]
           [label ""]
           [auto-resize #t]))

    (define (apply-styles!)
      (define t (current-theme))
      (define fg (parse-color (theme-separator-color t)))
      (for ([lbl (list state-label tool-label cost-label)])
        (send lbl set-color fg)))

    (apply-styles!)

    (define/public (apply-theme)
      (apply-styles!))

    (define/public (set-state state-sym)
      (send state-label set-label
            (format "State: ~a" state-sym)))

    (define/public (set-tool-name name)
      (send tool-label set-label
            (if name (format "  Tool: ~a" name) "")))

    (define/public (set-cost cost-usd)
      (when cost-usd
        (send cost-label set-label
              (format "  Cost: $~a" cost-usd))))))
