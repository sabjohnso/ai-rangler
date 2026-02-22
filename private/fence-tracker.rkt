#lang racket/base

;; Fence tracker — streaming markdown code-fence detection.
;; Pure functional: no dependencies beyond racket/base.
;;
;; Processes text deltas character-by-character (line-by-line),
;; detecting ``` fence markers and emitting tagged segments:
;;   'prose      — text outside code fences
;;   'code       — text inside code fences
;;   'fence-open — opening fence (text = language name or "")
;;   'fence-close — closing fence (text = "")

(require racket/string)

(provide fence-state
         fence-state?
         fence-state-in-code?
         fence-state-language
         fence-state-pending
         fence-state-init
         fence-track
         fence-flush)

;; ─── State ────────────────────────────────────────────────────────────────────

(struct fence-state (in-code? language pending) #:transparent)

(define fence-state-init (fence-state #f #f ""))

;; ─── Internal: check if a line is a fence marker ──────────────────────────────

;; A fence line is one that matches ^```(.*)$  (the line content without \n).
(define (fence-line? line)
  (and (>= (string-length line) 3)
       (char=? (string-ref line 0) #\`)
       (char=? (string-ref line 1) #\`)
       (char=? (string-ref line 2) #\`)
       ;; No backticks allowed in the info string (closing fences are just ```)
       (not (for/or ([i (in-range 3 (string-length line))])
              (char=? (string-ref line i) #\`)))))

;; Extract language from opening fence info string.
(define (fence-language line)
  (string-trim (substring line 3)))

;; ─── Split buffered text into complete lines + remainder ──────────────────────

;; Returns (values complete-lines remainder)
;; where complete-lines is a list of strings (each ending with \n)
;; and remainder is the trailing partial line (no \n).
(define (split-lines text)
  (let loop ([pos 0] [start 0] [lines '()])
    (cond
      [(>= pos (string-length text))
       (values (reverse lines)
               (substring text start))]
      [(char=? (string-ref text pos) #\newline)
       (loop (add1 pos)
             (add1 pos)
             (cons (substring text start (add1 pos)) lines))]
      [else
       (loop (add1 pos) start lines)])))

;; ─── Core: process a text delta ───────────────────────────────────────────────

(define (fence-track state delta)
  (define buffered (string-append (fence-state-pending state) delta))
  (define-values (lines remainder) (split-lines buffered))

  (let loop ([lines lines]
             [in-code? (fence-state-in-code? state)]
             [language (fence-state-language state)]
             [segments '()]
             [accum ""])
    (cond
      [(null? lines)
       ;; Flush accumulated text as a segment, carry remainder as pending
       (define final-segs
         (if (string=? accum "")
             (reverse segments)
             (reverse (cons (cons (if in-code? 'code 'prose) accum)
                            segments))))
       (values final-segs
               (fence-state in-code? language remainder))]
      [else
       (define line (car lines))
       (define line-content (substring line 0 (sub1 (string-length line)))) ;; strip \n
       (cond
         ;; Opening fence (not currently in code)
         [(and (not in-code?) (fence-line? line-content))
          (define lang (fence-language line-content))
          ;; Flush any accumulated prose before the fence
          (define new-segs
            (if (string=? accum "")
                (cons (cons 'fence-open lang) segments)
                (cons (cons 'fence-open lang)
                      (cons (cons 'prose accum) segments))))
          (loop (cdr lines) #t lang new-segs "")]

         ;; Closing fence (currently in code)
         [(and in-code? (fence-line? line-content)
               (string=? (fence-language line-content) ""))
          ;; Flush accumulated code, then emit fence-close
          (define new-segs
            (if (string=? accum "")
                (cons (cons 'fence-close "") segments)
                (cons (cons 'fence-close "")
                      (cons (cons 'code accum) segments))))
          (loop (cdr lines) #f #f new-segs "")]

         ;; Regular line — accumulate
         [else
          (loop (cdr lines) in-code? language segments
                (string-append accum line))])])))

;; ─── Flush pending buffer ─────────────────────────────────────────────────────

(define (fence-flush state)
  (define pending (fence-state-pending state))
  (cond
    [(string=? pending "")
     (values '() fence-state-init)]
    [else
     (define tag (if (fence-state-in-code? state) 'code 'prose))
     (values (list (cons tag pending))
             fence-state-init)]))
