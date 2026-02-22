#lang racket/base

(require rackunit
         "../private/config.rkt")

;; ─── config-path ────────────────────────────────────────────────────────────

(test-case "config-path uses XDG_CONFIG_HOME when set"
  (define env (make-environment-variables))
  (environment-variables-set! env #"XDG_CONFIG_HOME" #"/tmp/xdg-test")
  (define p (config-path #:env env))
  (check-equal? p "/tmp/xdg-test/ai-rangler/config.rkt"))

(test-case "config-path falls back to ~/.config when XDG not set"
  (define env (make-environment-variables))
  (environment-variables-set! env #"HOME" #"/home/testuser")
  (define p (config-path #:env env))
  (check-equal? p "/home/testuser/.config/ai-rangler/config.rkt"))

;; ─── default-config ─────────────────────────────────────────────────────────

(test-case "default-config has expected keys with default values"
  (define cfg (default-config))
  (check-true (config? cfg))
  (check-equal? (config-ref cfg 'claude-path) #f)
  (check-equal? (config-ref cfg 'default-cwd) #f)
  (check-equal? (config-ref cfg 'default-model) #f)
  (check-equal? (config-ref cfg 'default-theme) "Light"))

;; ─── load-config ────────────────────────────────────────────────────────────

(test-case "load-config returns default-config when file does not exist"
  (define cfg (load-config #:path "/tmp/nonexistent-config-12345.rkt"))
  (check-true (config? cfg))
  (check-equal? (config-ref cfg 'claude-path) #f)
  (check-equal? (config-ref cfg 'default-theme) "Light"))

(test-case "load-config reads a valid alist file"
  (define tmp-path "/tmp/test-config-valid.rkt")
  (call-with-output-file tmp-path
    (λ (out)
      (write '((claude-path . "/usr/bin/claude")
               (default-theme . "Dark"))
             out))
    #:exists 'replace)
  (define cfg (load-config #:path tmp-path))
  (check-equal? (config-ref cfg 'claude-path) "/usr/bin/claude")
  (check-equal? (config-ref cfg 'default-theme) "Dark")
  ;; Unset keys keep defaults
  (check-equal? (config-ref cfg 'default-cwd) #f)
  (delete-file tmp-path))

(test-case "load-config falls back on malformed file"
  (define tmp-path "/tmp/test-config-malformed.rkt")
  (call-with-output-file tmp-path
    (λ (out) (display "not valid s-expression {{{{" out))
    #:exists 'replace)
  (define cfg (load-config #:path tmp-path))
  (check-true (config? cfg))
  (check-equal? (config-ref cfg 'default-theme) "Light")
  (delete-file tmp-path))

(test-case "load-config rejects non-symbol keys gracefully"
  (define tmp-path "/tmp/test-config-badkeys.rkt")
  (call-with-output-file tmp-path
    (λ (out)
      (write '(("string-key" . "value")
               (claude-path . "/usr/bin/claude"))
             out))
    #:exists 'replace)
  ;; Non-symbol keys are skipped; valid entries still load
  (define cfg (load-config #:path tmp-path))
  (check-equal? (config-ref cfg 'claude-path) "/usr/bin/claude")
  (delete-file tmp-path))

;; ─── config-ref ─────────────────────────────────────────────────────────────

(test-case "config-ref returns #f for unknown keys"
  (define cfg (default-config))
  (check-equal? (config-ref cfg 'nonexistent-key) #f))

;; ─── merge-cli-config ───────────────────────────────────────────────────────

(test-case "merge-cli-config overrides with non-#f CLI values"
  (define cfg (default-config))
  (define merged (merge-cli-config cfg
                                   #:cwd "/projects"
                                   #:model "opus"
                                   #:claude-path "/usr/bin/claude"
                                   #:theme "Dark"))
  (check-equal? (config-ref merged 'default-cwd) "/projects")
  (check-equal? (config-ref merged 'default-model) "opus")
  (check-equal? (config-ref merged 'claude-path) "/usr/bin/claude")
  (check-equal? (config-ref merged 'default-theme) "Dark"))

(test-case "merge-cli-config with #f preserves existing config values"
  (define tmp-path "/tmp/test-config-merge.rkt")
  (call-with-output-file tmp-path
    (λ (out)
      (write '((claude-path . "/custom/claude")
               (default-theme . "Dark"))
             out))
    #:exists 'replace)
  (define cfg (load-config #:path tmp-path))
  (define merged (merge-cli-config cfg))
  ;; All #f by default — should preserve loaded values
  (check-equal? (config-ref merged 'claude-path) "/custom/claude")
  (check-equal? (config-ref merged 'default-theme) "Dark")
  (delete-file tmp-path))
