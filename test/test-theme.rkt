#lang racket/base

(require rackunit
         racket/class
         racket/gui/base
         "../private/gui/theme.rkt")

;; ─── Struct construction and access ─────────────────────────────────────────

(test-case "light-theme has expected name"
  (check-equal? (theme-name light-theme) "Light"))

(test-case "dark-theme has expected name"
  (check-equal? (theme-name dark-theme) "Dark"))

(test-case "light and dark themes are distinct"
  (check-not-equal? (theme-background light-theme)
                    (theme-background dark-theme)))

(test-case "theme struct fields are accessible"
  (define t (theme "Test" "#ffffff" "blue" "black"
                   "dark green" "red" "gray"
                   "#f5f5f5" (code-colors "#0000ff" "#a31515" "#008000"
                                          "#098658" "#000000" "#7f0055"
                                          "#ff0000" "#333333")
                   "#e0e0e0" "#999999"))
  (check-equal? (theme-name t) "Test")
  (check-equal? (theme-background t) "#ffffff")
  (check-equal? (theme-user-color t) "blue")
  (check-equal? (theme-assistant-color t) "black")
  (check-equal? (theme-tool-color t) "dark green")
  (check-equal? (theme-error-color t) "red")
  (check-equal? (theme-separator-color t) "gray")
  (check-equal? (theme-fence-background t) "#e0e0e0")
  (check-equal? (theme-fence-color t) "#999999"))

(test-case "light-theme has fence-background and fence-color"
  (check-equal? (theme-fence-background light-theme) "#e0e0e0")
  (check-equal? (theme-fence-color light-theme) "#999999"))

(test-case "dark-theme has fence-background and fence-color"
  (check-equal? (theme-fence-background dark-theme) "#383838")
  (check-equal? (theme-fence-color dark-theme) "#808080"))

;; ─── current-theme parameter ────────────────────────────────────────────────

(test-case "current-theme defaults to light-theme"
  (check-equal? (current-theme) light-theme))

(test-case "current-theme can be switched to dark"
  (parameterize ([current-theme dark-theme])
    (check-equal? (current-theme) dark-theme))
  ;; reverts outside parameterize
  (check-equal? (current-theme) light-theme))

;; ─── parse-color ────────────────────────────────────────────────────────────

(test-case "parse-color handles named colors"
  (define c (parse-color "blue"))
  (check-true (is-a? c color%))
  (check-equal? (send c red) 0)
  (check-equal? (send c green) 0)
  (check-equal? (send c blue) 255))

(test-case "parse-color handles hex #RRGGBB"
  (define c (parse-color "#1e1e1e"))
  (check-true (is-a? c color%))
  (check-equal? (send c red) #x1e)
  (check-equal? (send c green) #x1e)
  (check-equal? (send c blue) #x1e))

(test-case "parse-color handles hex #rrggbb uppercase"
  (define c (parse-color "#FF8000"))
  (check-equal? (send c red) 255)
  (check-equal? (send c green) 128)
  (check-equal? (send c blue) 0))

(test-case "parse-color handles white"
  (define c (parse-color "white"))
  (check-true (is-a? c color%))
  (check-equal? (send c red) 255)
  (check-equal? (send c green) 255)
  (check-equal? (send c blue) 255))
