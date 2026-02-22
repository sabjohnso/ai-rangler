#lang racket/base

;; Chat view — displays conversation messages in a scrollable text editor.
;; Provides methods to append user messages, assistant text (streaming),
;; and tool notifications with styled formatting.
;; Styles are driven by the current-theme parameter.

(require racket/class
         racket/gui/base
         "theme.rkt")

(provide chat-view%)

(define chat-view%
  (class vertical-panel%
    (super-new)

    ;; The text editor and its canvas for display.
    (define editor (new text%))
    (send editor auto-wrap #t)
    (define canvas (new editor-canvas%
                       [parent this]
                       [editor editor]
                       [style '(no-hscroll auto-vscroll)]))

    ;; Style definitions — set up once.
    (send editor change-style
          (make-object style-delta% 'change-family 'modern)
          0 0)

    ;; ─── Style helpers ───────────────────────────────────────────────────

    (define (make-style-delta #:color [color #f] #:bold? [bold? #f])
      (define sd (new style-delta%))
      (when bold? (send sd set-weight-on 'bold))
      (when color
        (define c (parse-color color))
        (send sd set-delta-foreground c))
      sd)

    ;; ─── Theme-driven styles ────────────────────────────────────────────

    (define user-style #f)
    (define assistant-style #f)
    (define tool-style #f)
    (define error-style #f)
    (define separator-style #f)

    (define (rebuild-styles!)
      (define t (current-theme))
      (set! user-style (make-style-delta #:color (theme-user-color t) #:bold? #t))
      (set! assistant-style (make-style-delta #:color (theme-assistant-color t)))
      (set! tool-style (make-style-delta #:color (theme-tool-color t) #:bold? #t))
      (set! error-style (make-style-delta #:color (theme-error-color t) #:bold? #t))
      (set! separator-style (make-style-delta #:color (theme-separator-color t))))

    (define (apply-background!)
      (send canvas set-canvas-background
            (parse-color (theme-background (current-theme)))))

    ;; Initialize styles and background at construction time.
    (rebuild-styles!)
    (apply-background!)

    ;; ─── Append helpers ──────────────────────────────────────────────────

    (define (append-styled text style)
      (define start (send editor last-position))
      (send editor insert text start)
      (define end (send editor last-position))
      (send editor change-style style start end)
      (scroll-to-end))

    (define (scroll-to-end)
      (send editor set-position (send editor last-position))
      (send canvas scroll-to 0 (send editor last-position) 0 0 #t))

    ;; ─── Public interface ────────────────────────────────────────────────

    (define/public (apply-theme)
      (rebuild-styles!)
      (apply-background!))

    (define/public (append-user-message text)
      (append-styled "\n--- You ---\n" separator-style)
      (append-styled (string-append text "\n") user-style))

    (define/public (append-assistant-text text)
      (append-styled text assistant-style))

    (define/public (begin-assistant-message)
      (append-styled "\n--- Assistant ---\n" separator-style))

    (define/public (append-tool-notification name action)
      (append-styled (format "\n[~a: ~a]\n" name action) tool-style))

    (define/public (append-error text)
      (append-styled (format "\n[Error: ~a]\n" text) error-style))

    (define/public (append-result text cost-usd)
      (when text
        (append-styled "\n" assistant-style))
      (when cost-usd
        (append-styled (format "\n--- Cost: $~a ---\n" cost-usd) separator-style)))))
