#lang racket/base

;; User configuration — loads an alist config file from disk,
;; provides typed access and CLI override merging.
;; Config is an immutable hasheq internally; an alist on disk.

(require racket/file)

(provide config?
         config-path
         default-config
         load-config
         config-ref
         merge-cli-config)

;; ─── Config representation ──────────────────────────────────────────────────

;; A config is just a hasheq. We use a predicate wrapper so
;; consumers can check types.
(define (config? v) (hash? v))

;; ─── Defaults ───────────────────────────────────────────────────────────────

(define default-keys
  '((claude-path   . #f)
    (default-cwd   . #f)
    (default-model . #f)
    (default-theme . "Light")))

(define (default-config)
  (make-hasheq-from-alist default-keys))

;; ─── Config path ────────────────────────────────────────────────────────────

;; Resolve XDG_CONFIG_HOME or fall back to ~/.config.
;; Accepts #:env for testability.
(define (config-path #:env [env (current-environment-variables)])
  (define xdg
    (let ([v (environment-variables-ref env #"XDG_CONFIG_HOME")])
      (and v (bytes->string/utf-8 v))))
  (define base
    (or xdg
        (let ([home (environment-variables-ref env #"HOME")])
          (if home
              (string-append (bytes->string/utf-8 home) "/.config")
              (expand-user-path "~/.config")))))
  (string-append (if (path? base) (path->string base) base)
                 "/ai-rangler/config.rkt"))

;; ─── Loading ────────────────────────────────────────────────────────────────

;; Read a config file at the given path. Returns a config hasheq.
;; Falls back to default-config on any error (missing file, parse error, etc).
(define (load-config #:path [path (config-path)])
  (with-handlers ([exn:fail? (λ (_) (default-config))])
    (define data (call-with-input-file path read))
    (validate-and-merge data)))

;; Validate an s-expression as an alist with symbol keys.
;; Skips invalid entries; merges valid entries over defaults.
(define (validate-and-merge data)
  (cond
    [(not (list? data)) (default-config)]
    [else
     (define base (default-config))
     (for/fold ([cfg base])
               ([entry (in-list data)])
       (cond
         [(and (pair? entry)
               (symbol? (car entry))
               (or (string? (cdr entry))
                   (not (cdr entry))))
          (hash-set cfg (car entry) (cdr entry))]
         [else cfg]))]))

;; ─── Access ─────────────────────────────────────────────────────────────────

;; Look up a config key. Returns #f for unknown keys.
(define (config-ref cfg key)
  (hash-ref cfg key #f))

;; ─── CLI merge ──────────────────────────────────────────────────────────────

;; Override config values with non-#f CLI arguments.
(define (merge-cli-config cfg
                          #:cwd [cwd #f]
                          #:model [model #f]
                          #:claude-path [claude-path #f]
                          #:theme [theme #f])
  (define overrides
    (filter cdr
            (list (cons 'default-cwd cwd)
                  (cons 'default-model model)
                  (cons 'claude-path claude-path)
                  (cons 'default-theme theme))))
  (for/fold ([c cfg])
            ([pair (in-list overrides)])
    (hash-set c (car pair) (cdr pair))))

;; ─── Helpers ────────────────────────────────────────────────────────────────

(define (make-hasheq-from-alist alist)
  (for/hasheq ([pair (in-list alist)])
    (values (car pair) (cdr pair))))
