#lang racket/base

;; Tests for table-tracker — streaming markdown table detection.

(require rackunit
         racket/list
         racket/string
         "../private/table-tracker.rkt")

;; ─── table-line? ─────────────────────────────────────────────────────────────

(test-case "table-line? accepts pipe-delimited lines"
  (check-true (table-line? "| a | b |"))
  (check-true (table-line? "| a | b | c |"))
  (check-true (table-line? "|a|b|"))
  (check-true (table-line? "| single |")))

(test-case "table-line? rejects non-table lines"
  (check-false (table-line? "just plain text"))
  (check-false (table-line? "| only opening pipe"))
  (check-false (table-line? "only closing pipe |"))
  (check-false (table-line? ""))
  (check-false (table-line? "|")))

;; ─── table-separator? ────────────────────────────────────────────────────────

(test-case "table-separator? accepts separator lines"
  (check-true (table-separator? "|---|---|"))
  (check-true (table-separator? "| --- | --- |"))
  (check-true (table-separator? "|:---|---:|"))
  (check-true (table-separator? "| :---: | --- |"))
  (check-true (table-separator? "|-------|-------|")))

(test-case "table-separator? rejects non-separator lines"
  (check-false (table-separator? "| a | b |"))
  (check-false (table-separator? "| --- | text |"))
  (check-false (table-separator? "just text"))
  (check-false (table-separator? "")))

;; ─── parse-table-cells ───────────────────────────────────────────────────────

(test-case "parse-table-cells splits pipe-delimited cells"
  (check-equal? (parse-table-cells "| a | b | c |") '("a" "b" "c"))
  (check-equal? (parse-table-cells "|a|b|c|") '("a" "b" "c"))
  (check-equal? (parse-table-cells "| hello world | test |")
                '("hello world" "test")))

(test-case "parse-table-cells trims whitespace"
  (check-equal? (parse-table-cells "|  spaced  |  out  |")
                '("spaced" "out")))

;; ─── parse-alignment ─────────────────────────────────────────────────────────

(test-case "parse-alignment detects column alignment"
  (check-equal? (parse-alignment "|---|---|") '(left left))
  (check-equal? (parse-alignment "|:---|---:|") '(left right))
  (check-equal? (parse-alignment "| :---: | --- |") '(center left))
  (check-equal? (parse-alignment "|:---:|:---:|") '(center center)))

;; ─── format-table ────────────────────────────────────────────────────────────

(test-case "format-table pads columns to equal width"
  (define rows '("| Name | Age |"
                 "|------|-----|"
                 "| Alice | 30 |"
                 "| Bob | 7 |"))
  (define result (format-table rows))
  ;; All data lines should have consistent column widths
  (define lines (regexp-split #rx"\n" result))
  ;; Filter out empty trailing line
  (define non-empty (filter (lambda (s) (> (string-length s) 0)) lines))
  ;; Should have 4 lines: header, separator, 2 data rows
  (check-equal? (length non-empty) 4)
  ;; All lines should start and end with |
  (for ([line (in-list non-empty)])
    (check-true (and (char=? (string-ref line 0) #\|)
                     (char=? (string-ref line (sub1 (string-length line))) #\|))
                (format "Line should be pipe-delimited: ~a" line))))

(test-case "format-table handles single-column table"
  (define rows '("| Item |"
                 "|------|"
                 "| Apple |"))
  (define result (format-table rows))
  (check-true (string? result))
  (check-true (> (string-length result) 0)))

(test-case "format-table preserves all data content"
  (define rows '("| X | Y |"
                 "|---|---|"
                 "| hello | world |"))
  (define result (format-table rows))
  (check-true (string-contains? result "hello"))
  (check-true (string-contains? result "world"))
  (check-true (string-contains? result "X"))
  (check-true (string-contains? result "Y")))

;; ─── State machine: idle → maybe-header ──────────────────────────────────────

(test-case "table line in idle mode enters maybe-header, emits nothing"
  (define-values (segs state)
    (table-track table-state-init "| Name | Age |\n"))
  (check-equal? segs '())
  (check-equal? (table-state-mode state) 'maybe-header))

;; ─── State machine: maybe-header + separator → in-table ──────────────────────

(test-case "separator after header confirms table, emits table-start + table-row"
  (define-values (segs1 state1)
    (table-track table-state-init "| Name | Age |\n"))
  (define-values (segs2 state2)
    (table-track state1 "|------|-----|\n"))
  ;; Should emit table-start (header) + table-row (separator)
  (check-equal? (length segs2) 2)
  (check-equal? (car (first segs2)) 'table-start)
  (check-equal? (car (second segs2)) 'table-row)
  (check-equal? (table-state-mode state2) 'in-table))

;; ─── State machine: maybe-header + non-separator → flush as prose ────────────

(test-case "non-separator after potential header flushes both as prose"
  (define-values (segs1 state1)
    (table-track table-state-init "| looks like header |\n"))
  (define-values (segs2 state2)
    (table-track state1 "but this is not a separator\n"))
  ;; Should emit both lines as prose
  (check-equal? (length segs2) 1)
  (check-equal? (car (first segs2)) 'prose)
  (check-true (string-contains? (cdr (first segs2)) "looks like header"))
  (check-true (string-contains? (cdr (first segs2)) "not a separator"))
  (check-equal? (table-state-mode state2) 'idle))

;; ─── State machine: in-table + table line → table-row ────────────────────────

(test-case "table row while in-table emits table-row"
  (define-values (segs1 state1)
    (table-track table-state-init "| H1 | H2 |\n"))
  (define-values (segs2 state2)
    (table-track state1 "|----|----|  \n"))
  (define-values (segs3 state3)
    (table-track state2 "| a  | b  |\n"))
  (check-equal? (length segs3) 1)
  (check-equal? (car (first segs3)) 'table-row)
  (check-equal? (table-state-mode state3) 'in-table))

;; ─── State machine: in-table + non-table line → table-end + prose ────────────

(test-case "non-table line while in-table ends table and emits prose"
  (define-values (segs1 state1)
    (table-track table-state-init "| H |\n"))
  (define-values (segs2 state2)
    (table-track state1 "|---|\n"))
  (define-values (segs3 state3)
    (table-track state2 "| data |\n"))
  (define-values (segs4 state4)
    (table-track state3 "regular text\n"))
  ;; Should emit table-end then prose
  (check-equal? (length segs4) 2)
  (check-equal? (car (first segs4)) 'table-end)
  (check-equal? (car (second segs4)) 'prose)
  (check-equal? (table-state-mode state4) 'idle))

;; ─── Complete table in one delta ─────────────────────────────────────────────

(test-case "complete table in single delta"
  (define input "| A | B |\n|---|---|\n| 1 | 2 |\n")
  (define-values (segs state) (table-track table-state-init input))
  ;; table-start, table-row (separator), table-row (data)
  (check-equal? (length segs) 3)
  (check-equal? (car (first segs)) 'table-start)
  (check-equal? (car (second segs)) 'table-row)
  (check-equal? (car (third segs)) 'table-row)
  (check-equal? (table-state-mode state) 'in-table))

;; ─── Prose passthrough ───────────────────────────────────────────────────────

(test-case "plain prose passes through unchanged"
  (define-values (segs state)
    (table-track table-state-init "Hello world\nSecond line\n"))
  (check-equal? (length segs) 1)
  (check-equal? (car (first segs)) 'prose)
  (check-equal? (cdr (first segs)) "Hello world\nSecond line\n")
  (check-equal? (table-state-mode state) 'idle))

;; ─── Flush ───────────────────────────────────────────────────────────────────

(test-case "flush in idle with pending emits prose"
  (define-values (segs1 state1)
    (table-track table-state-init "no newline"))
  (check-equal? segs1 '())
  (define-values (segs2 state2) (table-flush state1))
  (check-equal? (length segs2) 1)
  (check-equal? (car (first segs2)) 'prose)
  (check-equal? (cdr (first segs2)) "no newline")
  (check-equal? (table-state-mode state2) 'idle))

(test-case "flush in maybe-header emits buffered header as prose"
  (define-values (segs1 state1)
    (table-track table-state-init "| potential header |\n"))
  (check-equal? (table-state-mode state1) 'maybe-header)
  (define-values (segs2 state2) (table-flush state1))
  (check-equal? (length segs2) 1)
  (check-equal? (car (first segs2)) 'prose)
  (check-true (string-contains? (cdr (first segs2)) "potential header"))
  (check-equal? (table-state-mode state2) 'idle))

(test-case "flush in in-table emits table-end"
  (define-values (segs1 state1)
    (table-track table-state-init "| H |\n"))
  (define-values (segs2 state2)
    (table-track state1 "|---|\n"))
  (check-equal? (table-state-mode state2) 'in-table)
  (define-values (segs3 state3) (table-flush state2))
  (check-equal? (length segs3) 1)
  (check-equal? (car (first segs3)) 'table-end)
  (check-equal? (table-state-mode state3) 'idle))

(test-case "flush with empty state emits nothing"
  (define-values (segs state) (table-flush table-state-init))
  (check-equal? segs '())
  (check-equal? state table-state-init))

;; ─── Table with surrounding prose ────────────────────────────────────────────

(test-case "prose before and after table"
  (define input "Some text\n| A | B |\n|---|---|\n| 1 | 2 |\nMore text\n")
  (define-values (segs state) (table-track table-state-init input))
  ;; prose "Some text\n", table-start, table-row (sep), table-row (data),
  ;; table-end, prose "More text\n"
  (check-equal? (length segs) 6)
  (check-equal? (car (first segs)) 'prose)
  (check-equal? (car (second segs)) 'table-start)
  (check-equal? (car (third segs)) 'table-row)
  (check-equal? (car (fourth segs)) 'table-row)
  (check-equal? (car (fifth segs)) 'table-end)
  (check-equal? (car (sixth segs)) 'prose))

;; ─── Partial line buffering ──────────────────────────────────────────────────

(test-case "partial line is buffered (no newline)"
  (define-values (segs state)
    (table-track table-state-init "| partial"))
  (check-equal? segs '())
  (check-equal? (table-state-pending state) "| partial"))
