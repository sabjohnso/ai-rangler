#lang racket/base

;; Highlighter — lexer registry and tokenization.
;; Provides a registry mapping language names to lexer functions
;; (following the syntax-color/DrRacket lexer contract) and a
;; tokenize function that runs a lexer over a string.

(require syntax-color/racket-lexer)

(provide (struct-out token)
         register-language!
         lookup-language
         tokenize)

;; ─── Token descriptor ─────────────────────────────────────────────────────────

;; start and end are 0-based character positions.
(struct token (start end type) #:transparent)

;; ─── Language registry ────────────────────────────────────────────────────────

;; Mutable hash: language name (string) → lexer procedure.
;; Lexer contract: port → (values text type paren start end)
;; where start/end are 1-based positions from the port.
(define registry (make-hash))

(define (register-language! name lexer-fn)
  (hash-set! registry name lexer-fn))

(define (lookup-language name)
  (hash-ref registry name #f))

;; ─── Built-in registrations ───────────────────────────────────────────────────

(register-language! "racket" racket-lexer)
(register-language! "scheme" racket-lexer)
(register-language! "rkt" racket-lexer)

;; ─── Tokenization ─────────────────────────────────────────────────────────────

;; Tokenize a string using the registered lexer for the given language.
;; Returns a list of token structs with 0-based positions.
;; Returns empty list if language is unknown.
(define (tokenize text language)
  (define lexer (lookup-language language))
  (cond
    [(not lexer) '()]
    [else
     (define port (open-input-string text))
     (port-count-lines! port)
     (let loop ([tokens '()])
       (define-values (tok-text tok-type _paren tok-start tok-end)
         (lexer port))
       (cond
         [(eq? tok-type 'eof)
          (reverse tokens)]
         [else
          ;; Lexer returns 1-based positions; convert to 0-based
          (loop (cons (token (sub1 tok-start) (sub1 tok-end) tok-type)
                      tokens))]))]))
