#lang racket/base

(require rackunit
         racket/port
         "../private/stderr-ring.rkt")

;; ─── Unit tests for stderr-ring ─────────────────────────────────────────────

(test-case "fresh ring has no lines"
  (define r (make-stderr-ring))
  (check-equal? (stderr-ring-lines r) '()))

(test-case "ring stores lines in order"
  (define r (make-stderr-ring #:max-lines 5))
  (stderr-ring-push! r "line-1")
  (stderr-ring-push! r "line-2")
  (stderr-ring-push! r "line-3")
  (check-equal? (stderr-ring-lines r) '("line-1" "line-2" "line-3")))

(test-case "ring wraps at capacity, oldest lines dropped"
  (define r (make-stderr-ring #:max-lines 3))
  (for ([i (in-range 5)])
    (stderr-ring-push! r (format "line-~a" i)))
  ;; Only last 3 should remain
  (check-equal? (stderr-ring-lines r) '("line-2" "line-3" "line-4")))

(test-case "ring at exact capacity returns all lines in order"
  (define r (make-stderr-ring #:max-lines 3))
  (stderr-ring-push! r "a")
  (stderr-ring-push! r "b")
  (stderr-ring-push! r "c")
  (check-equal? (stderr-ring-lines r) '("a" "b" "c")))

(test-case "start-stderr-drain reads lines from port into ring"
  (define r (make-stderr-ring #:max-lines 10))
  (define-values (in out) (make-pipe))
  (define t (start-stderr-drain r in))
  (displayln "hello" out)
  (displayln "world" out)
  (close-output-port out)
  (thread-wait t)
  (check-equal? (stderr-ring-lines r) '("hello" "world")))

(test-case "drain handles empty port gracefully"
  (define r (make-stderr-ring))
  (define-values (in out) (make-pipe))
  (close-output-port out)
  (define t (start-stderr-drain r in))
  (thread-wait t)
  (check-equal? (stderr-ring-lines r) '()))
