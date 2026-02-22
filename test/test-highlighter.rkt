#lang racket/base

;; Tests for highlighter — lexer registry and tokenization.

(require rackunit
         racket/list
         "../private/highlighter.rkt")

;; ─── Tokenize Racket code ─────────────────────────────────────────────────────

(test-case "tokenize (define x 42) → correct token types and positions"
  (define tokens (tokenize "(define x 42)" "racket"))
  (check-true (not (null? tokens)))
  ;; First token: opening paren
  (check-equal? (token-type (first tokens)) 'parenthesis)
  (check-equal? (token-start (first tokens)) 0)
  (check-equal? (token-end (first tokens)) 1)
  ;; "define" is a symbol
  (define sym-tok (second tokens))
  (check-equal? (token-type sym-tok) 'symbol)
  (check-equal? (token-start sym-tok) 1)
  (check-equal? (token-end sym-tok) 7)
  ;; "42" is a constant
  (define const-tok (findf (lambda (t) (eq? (token-type t) 'constant)) tokens))
  (check-true (token? const-tok))
  (check-equal? (token-start const-tok) 10)
  (check-equal? (token-end const-tok) 12))

;; ─── String literal ───────────────────────────────────────────────────────────

(test-case "tokenize string literal → string token"
  (define tokens (tokenize "\"hello world\"" "racket"))
  (define str-tok (findf (lambda (t) (eq? (token-type t) 'string)) tokens))
  (check-true (token? str-tok)))

;; ─── Comment ──────────────────────────────────────────────────────────────────

(test-case "tokenize comment → comment token"
  (define tokens (tokenize "; a comment\n" "racket"))
  (define cmt-tok (findf (lambda (t) (eq? (token-type t) 'comment)) tokens))
  (check-true (token? cmt-tok)))

;; ─── Unknown language ─────────────────────────────────────────────────────────

(test-case "unknown language returns empty list"
  (define tokens (tokenize "some code" "brainfuck"))
  (check-equal? tokens '()))

;; ─── Register custom lexer ────────────────────────────────────────────────────

(test-case "register and use custom lexer"
  ;; A trivial lexer that tags everything as 'symbol
  (define (trivial-lexer port)
    (define c (read-string 1 port))
    (if (eof-object? c)
        (values eof 'eof #f #f #f)
        (let ([pos (file-position port)])
          (values c 'symbol #f (sub1 pos) pos))))
  (register-language! "trivial" trivial-lexer)
  (define tokens (tokenize "ab" "trivial"))
  (check-equal? (length tokens) 2)
  (check-equal? (token-type (first tokens)) 'symbol)
  (check-equal? (token-type (second tokens)) 'symbol))

;; ─── Property: token spans cover entire input without gaps or overlaps ────────

(test-case "property: tokens cover entire input without gaps"
  (define text "(define (f x) (+ x 1))")
  (define tokens (tokenize text "racket"))
  ;; Filter out whitespace tokens for gap check — actually let's check all
  ;; Tokens should be sorted by start position
  (define sorted (sort tokens < #:key token-start))
  ;; Check: first token starts at 0, last token ends at string length
  (check-equal? (token-start (first sorted)) 0)
  (check-equal? (token-end (last sorted)) (string-length text))
  ;; Check no gaps or overlaps: each token starts where previous ended
  (for ([prev (in-list sorted)]
        [curr (in-list (cdr sorted))])
    (check-equal? (token-end prev) (token-start curr)
                  (format "gap or overlap between ~a and ~a" prev curr))))

;; ─── Lookup ───────────────────────────────────────────────────────────────────

(test-case "lookup-language returns #f for unknown"
  (check-false (lookup-language "nonexistent")))

(test-case "lookup-language returns procedure for registered"
  (check-true (procedure? (lookup-language "racket"))))
