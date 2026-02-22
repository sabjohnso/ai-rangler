#lang racket/base

;; Theme — bundles semantic colors into a switchable data type.
;; Provides light and dark built-in themes, a current-theme parameter,
;; and a parse-color helper for named and hex color strings.

(require racket/class
         racket/gui/base
         racket/string)

(provide (struct-out theme)
         light-theme
         dark-theme
         current-theme
         parse-color)

(struct theme (name background user-color assistant-color
               tool-color error-color separator-color) #:transparent)

(define light-theme
  (theme "Light" "white" "blue" "black"
         "dark green" "red" "gray"))

(define dark-theme
  (theme "Dark" "#1e1e1e" "#6cb4ee" "#d4d4d4"
         "#4ec9b0" "#f44747" "#6a6a6a"))

(define current-theme (make-parameter light-theme))

;; parse-color : string? -> (is-a?/c color%)
;; Accepts named colors ("blue") or hex strings ("#RRGGBB").
(define (parse-color str)
  (cond
    [(and (string-prefix? str "#")
          (= (string-length str) 7))
     (define r (string->number (substring str 1 3) 16))
     (define g (string->number (substring str 3 5) 16))
     (define b (string->number (substring str 5 7) 16))
     (make-object color% r g b)]
    [else
     (define c (send the-color-database find-color str))
     (or c (make-object color% 0 0 0))]))
