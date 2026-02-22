#lang racket/base

;; Tests for fence-tracker — streaming markdown fence detection.

(require rackunit
         racket/list
         "../private/fence-tracker.rkt")

;; ─── Helpers ──────────────────────────────────────────────────────────────────

;; Collect all segment texts (excluding fence markers) from a list of segments.
(define (segment-texts segs)
  (for/list ([s (in-list segs)]
             #:when (or (eq? (car s) 'prose) (eq? (car s) 'code)))
    (cdr s)))

;; Concatenate all prose/code segment texts (excluding fence markers).
(define (segments->text segs)
  (apply string-append
         (for/list ([s (in-list segs)]
                    #:when (or (eq? (car s) 'prose) (eq? (car s) 'code)))
           (cdr s))))

;; ─── No fences (pure prose passthrough) ───────────────────────────────────────

(test-case "pure prose passes through unchanged"
  (define-values (segs state) (fence-track fence-state-init "Hello world\n"))
  (check-equal? segs '((prose . "Hello world\n")))
  (check-false (fence-state-in-code? state)))

(test-case "multiple prose lines in one delta"
  (define-values (segs state)
    (fence-track fence-state-init "line one\nline two\n"))
  (check-equal? segs '((prose . "line one\nline two\n")))
  (check-false (fence-state-in-code? state)))

;; ─── Complete fence in single delta ───────────────────────────────────────────

(test-case "complete code fence in one delta"
  (define-values (segs state)
    (fence-track fence-state-init "```racket\n(define x 42)\n```\n"))
  ;; Should produce: fence-open, code text, fence-close
  (check-equal? (length segs) 3)
  (check-equal? (car (first segs)) 'fence-open)
  (check-equal? (cdr (first segs)) "racket")
  (check-equal? (second segs) '(code . "(define x 42)\n"))
  (check-equal? (car (third segs)) 'fence-close)
  (check-false (fence-state-in-code? state)))

;; ─── Fence split across two deltas ────────────────────────────────────────────

(test-case "fence opening split across deltas"
  ;; First delta: partial fence line (no newline yet)
  (define-values (segs1 state1) (fence-track fence-state-init "```rac"))
  (check-equal? segs1 '())  ;; pending, no complete line
  ;; Second delta: complete the fence line
  (define-values (segs2 state2) (fence-track state1 "ket\ncode here\n"))
  ;; Now we should get fence-open + code
  (check-equal? (car (first segs2)) 'fence-open)
  (check-equal? (cdr (first segs2)) "racket")
  (check-equal? (second segs2) '(code . "code here\n"))
  (check-true (fence-state-in-code? state2)))

(test-case "fence closing split across deltas"
  ;; Start inside a code block
  (define open-state (fence-state #t "racket" ""))
  (define-values (segs1 state1) (fence-track open-state "last line\n``"))
  ;; "last line\n" is code, "``" is pending
  (check-equal? (first segs1) '(code . "last line\n"))
  ;; Complete the closing fence
  (define-values (segs2 state2) (fence-track state1 "`\nafter\n"))
  (check-equal? (car (first segs2)) 'fence-close)
  (check-equal? (second segs2) '(prose . "after\n"))
  (check-false (fence-state-in-code? state2)))

;; ─── Multiple fences in one delta ─────────────────────────────────────────────

(test-case "two code blocks in one delta"
  (define-values (segs state)
    (fence-track fence-state-init
                 "```python\nprint('hi')\n```\ntext\n```js\nconsole.log(1)\n```\n"))
  ;; fence-open, code, fence-close, prose, fence-open, code, fence-close
  (check-equal? (length segs) 7)
  (check-equal? (cdr (first segs)) "python")
  (check-equal? (second segs) '(code . "print('hi')\n"))
  (check-equal? (car (third segs)) 'fence-close)
  (check-equal? (fourth segs) '(prose . "text\n"))
  (check-equal? (cdr (fifth segs)) "js")
  (check-equal? (sixth segs) '(code . "console.log(1)\n"))
  (check-equal? (car (seventh segs)) 'fence-close)
  (check-false (fence-state-in-code? state)))

;; ─── Language extraction ──────────────────────────────────────────────────────

(test-case "opening fence without language"
  (define-values (segs state)
    (fence-track fence-state-init "```\ncode\n"))
  (check-equal? (cdr (first segs)) "")  ;; empty language
  (check-true (fence-state-in-code? state)))

(test-case "opening fence with language and whitespace"
  (define-values (segs state)
    (fence-track fence-state-init "```  scheme  \ncode\n"))
  ;; Language should be trimmed
  (check-equal? (cdr (first segs)) "scheme"))

;; ─── Flush ────────────────────────────────────────────────────────────────────

(test-case "flush emits pending prose"
  (define-values (segs1 state1)
    (fence-track fence-state-init "no newline yet"))
  (check-equal? segs1 '())  ;; pending
  (define-values (segs2 state2) (fence-flush state1))
  (check-equal? segs2 '((prose . "no newline yet")))
  (check-equal? state2 fence-state-init))

(test-case "flush emits pending code"
  (define in-code (fence-state #t "racket" "partial code"))
  (define-values (segs state) (fence-flush in-code))
  (check-equal? segs '((code . "partial code")))
  ;; Flush resets state
  (check-false (fence-state-in-code? state))
  (check-equal? (fence-state-pending state) ""))

(test-case "flush with empty pending emits nothing"
  (define-values (segs state) (fence-flush fence-state-init))
  (check-equal? segs '())
  (check-equal? state fence-state-init))

;; ─── Property: segment texts concatenation equals input minus fence lines ─────

(test-case "property: segment texts reconstruct non-fence content"
  (define input "before\n```racket\n(+ 1 2)\n```\nafter\n")
  (define-values (segs state) (fence-track fence-state-init input))
  (define-values (flush-segs _) (fence-flush state))
  (define all-segs (append segs flush-segs))
  ;; All non-fence text should equal input minus the fence lines
  (define reconstructed (segments->text all-segs))
  (check-equal? reconstructed "before\n(+ 1 2)\nafter\n"))
