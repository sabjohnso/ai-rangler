#lang racket/base

;; Chat view — displays conversation messages in a scrollable text editor.
;; Provides methods to append user messages, assistant text (streaming),
;; tool notifications, and syntax-highlighted code blocks.
;; Styles are driven by the current-theme parameter.
;;
;; Code blocks render with full-width background bands via highlighted-text%,
;; a text% subclass that paints background rectangles in on-paint.
;; Fence separators (opening/closing bars) are visible lines with their own
;; full-width background.

(require racket/class
         racket/gui/base
         racket/string
         "theme.rkt"
         "../highlighter.rkt"
         "../table-tracker.rkt")

(provide chat-view%)

;; ─── highlighted-text% ───────────────────────────────────────────────────
;;
;; Subclass of text% that maintains lists of character-position ranges
;; for code blocks and fence separators.  During on-paint (before? = #t),
;; it draws full-width filled rectangles at the y-positions of those
;; ranges, so the background spans the entire editor width rather than
;; only behind individual glyphs.

(define highlighted-text%
  (class text%
    (super-new)

    ;; Accumulators: lists of (cons start-pos end-pos) in the editor.
    (define code-ranges  '())
    (define fence-ranges '())

    ;; Colors — updated when theme changes.
    (define code-bg-color  (parse-color (theme-code-background (current-theme))))
    (define fence-bg-color (parse-color (theme-fence-background (current-theme))))

    (define/public (update-bg-colors!)
      (define t (current-theme))
      (set! code-bg-color  (parse-color (theme-code-background t)))
      (set! fence-bg-color (parse-color (theme-fence-background t))))

    (define/public (add-code-range! start end)
      (set! code-ranges (cons (cons start end) code-ranges)))

    (define/public (add-fence-range! start end)
      (set! fence-ranges (cons (cons start end) fence-ranges)))

    ;; Draw full-width background rectangles for a list of ranges.
    ;; paint-left/paint-right are the repaint region in editor coordinates,
    ;; supplied by on-paint, so the background spans the full visible width.
    (define (draw-bg-ranges! dc ranges color dx dy paint-left paint-right)
      (define old-brush (send dc get-brush))
      (define old-pen   (send dc get-pen))
      (send dc set-brush color 'solid)
      (send dc set-pen color 1 'transparent)
      (for ([range (in-list ranges)])
        (define start (car range))
        (define end   (cdr range))
        (define top-y (box 0.0))
        (define bot-y (box 0.0))
        (send this position-location start #f top-y #t)
        (send this position-location end #f bot-y #f)
        (define y1 (unbox top-y))
        (define y2 (unbox bot-y))
        (when (< y1 y2)
          (send dc draw-rectangle
                (+ paint-left dx) (+ y1 dy)
                (- paint-right paint-left) (- y2 y1))))
      (send dc set-brush old-brush)
      (send dc set-pen old-pen))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (when before?
        (draw-bg-ranges! dc code-ranges  code-bg-color  dx dy left right)
        (draw-bg-ranges! dc fence-ranges fence-bg-color dx dy left right))
      (super on-paint before? dc left top right bottom dx dy draw-caret))))

;; ─── chat-view% ──────────────────────────────────────────────────────────

(define chat-view%
  (class vertical-panel%
    (super-new)

    ;; The text editor and its canvas for display.
    (define editor (new highlighted-text%))
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

    (define (make-style-delta #:color [color #f] #:bold? [bold? #f]
                              #:background [background #f])
      (define sd (new style-delta%))
      (when bold? (send sd set-weight-on 'bold))
      (when color
        (define c (parse-color color))
        (send sd set-delta-foreground c))
      (when background
        (define c (parse-color background))
        (send sd set-delta-background c))
      sd)

    ;; ─── Theme-driven styles ────────────────────────────────────────────

    (define user-style #f)
    (define assistant-style #f)
    (define tool-style #f)
    (define error-style #f)
    (define separator-style #f)

    ;; Code block styles — foreground only; background handled by on-paint
    (define code-style #f)
    (define fence-style #f)          ; fence separator label style
    (define token-styles (hasheq))   ; token-type → style-delta

    (define (rebuild-styles!)
      (define t (current-theme))
      (set! user-style (make-style-delta #:color (theme-user-color t) #:bold? #t))
      (set! assistant-style (make-style-delta #:color (theme-assistant-color t)))
      (set! tool-style (make-style-delta #:color (theme-tool-color t) #:bold? #t))
      (set! error-style (make-style-delta #:color (theme-error-color t) #:bold? #t))
      (set! separator-style (make-style-delta #:color (theme-separator-color t)))
      ;; Fence separator style
      (set! fence-style (make-style-delta #:color (theme-fence-color t)))
      ;; Code block styles — foreground only (no per-character background)
      (define cc (theme-code-colors t))
      (set! code-style (make-style-delta #:color (code-colors-default cc)))
      (set! token-styles
        (hasheq 'symbol      (make-style-delta #:color (code-colors-symbol cc))
                'string      (make-style-delta #:color (code-colors-string cc))
                'comment     (make-style-delta #:color (code-colors-comment cc))
                'constant    (make-style-delta #:color (code-colors-constant cc))
                'parenthesis (make-style-delta #:color (code-colors-parenthesis cc))
                'keyword     (make-style-delta #:color (code-colors-keyword cc))
                'error       (make-style-delta #:color (code-colors-error cc))
                'white-space (make-style-delta)
                'other       (make-style-delta #:color (code-colors-default cc)))))

    (define (apply-background!)
      (send canvas set-canvas-background
            (parse-color (theme-background (current-theme))))
      (send editor update-bg-colors!))

    ;; Initialize styles and background at construction time.
    (rebuild-styles!)
    (apply-background!)

    ;; ─── Code block state ───────────────────────────────────────────────

    (define code-block-start #f)      ; editor position where code content began
    (define code-block-language #f)   ; language string or #f

    ;; ─── Table state ────────────────────────────────────────────────────

    (define table-start-pos #f)       ; editor position where table began
    (define table-rows '())           ; accumulated raw row strings

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

    ;; Insert a fence separator line and register its range.
    ;; The fence range excludes the trailing newline so that
    ;; on-paint draws a single-line background rectangle.
    (define (insert-fence-separator! label)
      (define start (send editor last-position))
      (define text (if (and label (not (string=? label "")))
                       (string-append " " label " \n")
                       "\n"))
      (send editor insert text start)
      (define end (send editor last-position))
      (send editor change-style fence-style start end)
      (send editor add-fence-range! start (- end 1))
      (scroll-to-end))

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

    (define/public (begin-code-block language)
      ;; Insert opening fence separator
      (insert-fence-separator! language)
      ;; Record where code content starts (after the fence line)
      (set! code-block-start (send editor last-position))
      (set! code-block-language (and language (not (string=? language ""))
                                     language)))

    (define/public (append-code-text text)
      (append-styled text code-style))

    (define/public (end-code-block)
      (when code-block-start
        (define block-end (send editor last-position))
        ;; Register the code content range for full-width background
        (send editor add-code-range! code-block-start block-end)
        ;; Apply per-token syntax highlighting (foreground only)
        (define text (send editor get-text code-block-start block-end))
        (define lang (or code-block-language ""))
        (define tokens (tokenize text lang))
        (for ([tok (in-list tokens)])
          (define tok-start (+ code-block-start (token-start tok)))
          (define tok-end   (+ code-block-start (token-end tok)))
          (define style (hash-ref token-styles (token-type tok)
                                 (hash-ref token-styles 'other #f)))
          (when style
            (send editor change-style style tok-start tok-end))))
      ;; Insert closing fence separator
      (insert-fence-separator! #f)
      (set! code-block-start #f)
      (set! code-block-language #f))

    (define/public (begin-table text)
      ;; Record start position, insert raw header with code-style, accumulate
      (set! table-start-pos (send editor last-position))
      (set! table-rows (list text))
      (append-styled text code-style))

    (define/public (append-table-row text)
      ;; Insert raw row with code-style, accumulate
      (set! table-rows (append table-rows (list text)))
      (append-styled text code-style))

    (define/public (end-table)
      (when table-start-pos
        (send editor begin-edit-sequence)
        ;; Delete the raw text we inserted during streaming
        (define current-end (send editor last-position))
        (send editor delete table-start-pos current-end)
        ;; Parse the raw rows (strip trailing newlines for format-table)
        (define raw-rows
          (for/list ([r (in-list table-rows)])
            (define s (string-trim r))
            s))
        ;; Format with computed column widths
        (define formatted (format-table raw-rows))
        ;; Insert formatted text at the same position
        (define insert-pos table-start-pos)
        (send editor insert formatted insert-pos)
        (define new-end (send editor last-position))
        ;; Apply code-style for foreground
        (send editor change-style code-style insert-pos new-end)
        ;; Register range for full-width background
        (send editor add-code-range! insert-pos new-end)
        (send editor end-edit-sequence)
        (scroll-to-end)
        ;; Reset state
        (set! table-start-pos #f)
        (set! table-rows '())))

    (define/public (append-tool-notification name action)
      (append-styled (format "\n[~a: ~a]\n" name action) tool-style))

    (define/public (append-error text)
      (append-styled (format "\n[Error: ~a]\n" text) error-style))

    (define/public (append-result text cost-usd)
      (when text
        (append-styled "\n" assistant-style))
      (when cost-usd
        (append-styled (format "\n--- Cost: $~a ---\n" cost-usd) separator-style)))))
