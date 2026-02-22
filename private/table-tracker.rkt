#lang racket/base

;; Table tracker — streaming markdown table detection.
;; Pure functional: no dependencies beyond racket/base.
;;
;; Processes prose text line by line, detecting markdown table
;; boundaries and emitting tagged segments:
;;   'prose       — text outside tables
;;   'table-start — confirmed header row (text = raw line)
;;   'table-row   — separator or data row (text = raw line)
;;   'table-end   — table boundary ended (text = "")
;;
;; Also provides helpers for parsing and formatting tables.

(require racket/string
         racket/list)

(provide table-state
         table-state?
         table-state-mode
         table-state-pending
         table-state-init
         table-track
         table-flush
         table-line?
         table-separator?
         parse-table-cells
         parse-alignment
         format-table)

;; ─── State ──────────────────────────────────────────────────────────────────

(struct table-state (mode pending-line pending) #:transparent)
;; mode: 'idle | 'maybe-header | 'in-table
;; pending-line: buffered potential header when mode = 'maybe-header, or #f
;; pending: partial line buffer (text without trailing newline)

(define table-state-init (table-state 'idle #f ""))

;; ─── Helpers: line classification ───────────────────────────────────────────

;; A table line starts and ends with | and has at least one cell.
(define (table-line? line)
  (define trimmed (string-trim line))
  (and (>= (string-length trimmed) 3)
       (char=? (string-ref trimmed 0) #\|)
       (char=? (string-ref trimmed (sub1 (string-length trimmed))) #\|)
       ;; Must have at least one inner pipe or content
       (> (length (string-split trimmed "|" #:trim? #t)) 0)))

;; A separator line is a table line where every cell matches [-: ]+
(define (table-separator? line)
  (and (table-line? line)
       (let ([cells (parse-table-cells line)])
         (and (pair? cells)
              (for/and ([cell (in-list cells)])
                (regexp-match? #rx"^:?-+:?$" (string-trim cell)))))))

;; ─── Helpers: parsing ───────────────────────────────────────────────────────

;; Split "| a | b | c |" → '("a" "b" "c")
(define (parse-table-cells line)
  (define trimmed (string-trim line))
  ;; Strip leading and trailing |
  (define inner
    (let* ([s (if (and (> (string-length trimmed) 0)
                       (char=? (string-ref trimmed 0) #\|))
                  (substring trimmed 1)
                  trimmed)]
           [s (if (and (> (string-length s) 0)
                       (char=? (string-ref s (sub1 (string-length s))) #\|))
                  (substring s 0 (sub1 (string-length s)))
                  s)])
      s))
  (map string-trim (string-split inner "|")))

;; Parse separator to determine column alignment.
;; Returns list of 'left, 'right, or 'center symbols.
(define (parse-alignment separator-line)
  (define cells (parse-table-cells separator-line))
  (for/list ([cell (in-list cells)])
    (define trimmed (string-trim cell))
    (define starts-colon? (and (> (string-length trimmed) 0)
                               (char=? (string-ref trimmed 0) #\:)))
    (define ends-colon? (and (> (string-length trimmed) 0)
                             (char=? (string-ref trimmed
                                                 (sub1 (string-length trimmed)))
                                     #\:)))
    (cond
      [(and starts-colon? ends-colon?) 'center]
      [ends-colon? 'right]
      [else 'left])))

;; ─── format-table ───────────────────────────────────────────────────────────

;; Given a list of raw row strings (header, separator, data rows),
;; compute column widths and return a fully padded/formatted string.
(define (format-table rows)
  (define all-cells (map parse-table-cells rows))
  ;; Find separator row to determine alignment
  (define sep-idx
    (for/first ([i (in-naturals)]
                [row (in-list rows)]
                #:when (table-separator? row))
      i))
  (define alignments
    (if sep-idx
        (parse-alignment (list-ref rows sep-idx))
        (make-list (if (pair? all-cells)
                       (length (first all-cells))
                       0)
                   'left)))
  ;; Compute max width per column
  (define num-cols (if (pair? all-cells) (apply max (map length all-cells)) 0))
  (define col-widths
    (for/list ([col (in-range num-cols)])
      (apply max 3  ; minimum width of 3 (for ---)
             (for/list ([cells (in-list all-cells)]
                        #:when (not (table-separator?
                                     (list-ref rows
                                               (for/first ([i (in-naturals)]
                                                           [c (in-list all-cells)]
                                                           #:when (eq? c cells))
                                                 i)))))
               (if (< col (length cells))
                   (string-length (list-ref cells col))
                   0)))))
  ;; Format each row
  (define formatted-lines
    (for/list ([row (in-list rows)]
               [cells (in-list all-cells)]
               [row-idx (in-naturals)])
      (if (and sep-idx (= row-idx sep-idx))
          ;; Format separator row
          (string-append
           "| "
           (string-join
            (for/list ([w (in-list col-widths)]
                       [a (in-list alignments)])
              (define dashes (make-string w #\-))
              (case a
                [(left)   dashes]
                [(right)  (string-append (substring dashes 1) ":")]
                [(center) (string-append ":" (substring dashes 2) ":")]))
            " | ")
           " |")
          ;; Format data/header row
          (string-append
           "| "
           (string-join
            (for/list ([col (in-range num-cols)]
                       [w (in-list col-widths)]
                       [a (in-list alignments)])
              (define cell (if (< col (length cells))
                               (list-ref cells col)
                               ""))
              (define pad (max 0 (- w (string-length cell))))
              (case a
                [(left)   (string-append cell (make-string pad #\space))]
                [(right)  (string-append (make-string pad #\space) cell)]
                [(center) (let* ([left-pad (quotient pad 2)]
                                 [right-pad (- pad left-pad)])
                            (string-append (make-string left-pad #\space)
                                           cell
                                           (make-string right-pad #\space)))]))
            " | ")
           " |"))))
  (string-append (string-join formatted-lines "\n") "\n"))

;; ─── Split buffered text into complete lines + remainder ────────────────────

(define (split-lines text)
  (let loop ([pos 0] [start 0] [lines '()])
    (cond
      [(>= pos (string-length text))
       (values (reverse lines)
               (substring text start))]
      [(char=? (string-ref text pos) #\newline)
       (loop (add1 pos)
             (add1 pos)
             (cons (substring text start pos) lines))]
      [else
       (loop (add1 pos) start lines)])))

;; ─── Core: process a text delta ─────────────────────────────────────────────

(define (table-track state delta)
  (define buffered (string-append (table-state-pending state) delta))
  (define-values (lines remainder) (split-lines buffered))

  (let loop ([lines lines]
             [mode (table-state-mode state)]
             [pending-line (table-state-pending-line state)]
             [segments '()]
             [accum ""])
    (cond
      [(null? lines)
       ;; Flush accumulated prose
       (define final-segs
         (if (string=? accum "")
             (reverse segments)
             (reverse (cons (cons 'prose accum) segments))))
       (values final-segs
               (table-state mode pending-line remainder))]
      [else
       (define line (car lines))
       (define line-with-nl (string-append line "\n"))
       (case mode
         [(idle)
          (cond
            [(table-line? line)
             ;; Potential header — buffer it, flush any accumulated prose
             (define new-segs
               (if (string=? accum "")
                   segments
                   (cons (cons 'prose accum) segments)))
             (loop (cdr lines) 'maybe-header line-with-nl new-segs "")]
            [else
             ;; Regular prose line
             (loop (cdr lines) 'idle #f segments
                   (string-append accum line-with-nl))])]

         [(maybe-header)
          (cond
            [(table-separator? line)
             ;; Confirmed table! Emit table-start for header, table-row for separator
             (define new-segs
               (cons (cons 'table-row line-with-nl)
                     (cons (cons 'table-start pending-line)
                           segments)))
             (loop (cdr lines) 'in-table #f new-segs "")]
            [else
             ;; Not a table — flush pending header as prose, reprocess current line
             (loop (cons line (cdr lines)) 'idle #f segments
                   (string-append accum pending-line))])]

         [(in-table)
          (cond
            [(table-line? line)
             ;; Another table row
             (define new-segs
               (cons (cons 'table-row line-with-nl) segments))
             (loop (cdr lines) 'in-table #f new-segs "")]
            [else
             ;; End of table, reprocess as idle
             (define new-segs (cons (cons 'table-end "") segments))
             (loop (cons line (cdr lines)) 'idle #f new-segs "")])])])))

;; ─── Flush pending buffer ───────────────────────────────────────────────────

(define (table-flush state)
  (define mode (table-state-mode state))
  (define pending-line (table-state-pending-line state))
  (define pending (table-state-pending state))

  (case mode
    [(idle)
     (if (string=? pending "")
         (values '() table-state-init)
         (values (list (cons 'prose pending))
                 table-state-init))]
    [(maybe-header)
     ;; Flush buffered header + any pending partial as prose
     (define text (string-append (or pending-line "") pending))
     (if (string=? text "")
         (values '() table-state-init)
         (values (list (cons 'prose text))
                 table-state-init))]
    [(in-table)
     ;; End the table, flush any pending partial
     (define segs
       (if (string=? pending "")
           (list (cons 'table-end ""))
           (list (cons 'table-end "") (cons 'prose pending))))
     (values segs table-state-init)]))
