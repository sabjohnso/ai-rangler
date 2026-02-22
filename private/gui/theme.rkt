#lang racket/base

;; Theme — bundles semantic colors into a switchable data type.
;; Provides light and dark built-in themes, a current-theme parameter,
;; and a parse-color helper for named and hex color strings.

(require racket/class
         racket/gui/base
         racket/string)

(provide (struct-out theme)
         (struct-out code-colors)
         light-theme
         dark-theme
         current-theme
         parse-color)

(struct code-colors
  (symbol string comment constant parenthesis keyword error default)
  #:transparent)

(struct theme (name background user-color assistant-color
               tool-color error-color separator-color
               code-background code-colors
               fence-background fence-color) #:transparent)

(define light-code-colors
  (code-colors "#0000ff"    ; symbol — blue
               "#a31515"    ; string — dark red
               "#008000"    ; comment — green
               "#098658"    ; constant — teal
               "#000000"    ; parenthesis — black
               "#7f0055"    ; keyword — purple
               "#ff0000"    ; error — red
               "#333333"))  ; default — dark grey

(define dark-code-colors
  (code-colors "#9cdcfe"    ; symbol — light blue
               "#ce9178"    ; string — light brown
               "#6a9955"    ; comment — green
               "#b5cea8"    ; constant — light green
               "#d4d4d4"    ; parenthesis — grey
               "#c586c0"    ; keyword — pink
               "#f44747"    ; error — red
               "#d4d4d4"))  ; default — grey

(define light-theme
  (theme "Light" "white" "blue" "black"
         "dark green" "red" "gray"
         "#f5f5f5" light-code-colors
         "#e0e0e0" "#999999"))

(define dark-theme
  (theme "Dark" "#1e1e1e" "#6cb4ee" "#d4d4d4"
         "#4ec9b0" "#f44747" "#6a6a6a"
         "#2d2d2d" dark-code-colors
         "#383838" "#808080"))

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
