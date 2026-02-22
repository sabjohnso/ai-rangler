#lang racket/base

(require rackunit
         json
         racket/runtime-path
         "../private/process.rkt")

(define-runtime-path mock-claude "helpers/mock-claude.rkt")

;; ─── Spawning and reading ────────────────────────────────────────────────────

(test-case "make-claude-process spawns and returns a claude-process struct"
  (define cp (make-claude-process
              #:command (list (find-executable-path "racket") (path->string mock-claude))))
  (check-true (claude-process? cp))
  (check-true (claude-process-running? cp))
  ;; Read the three lines of output
  (define line1 (read-json (claude-process-stdout cp)))
  (check-equal? (hash-ref line1 'type) "system")
  (define line2 (read-json (claude-process-stdout cp)))
  (check-equal? (hash-ref line2 'type) "assistant")
  (define line3 (read-json (claude-process-stdout cp)))
  (check-equal? (hash-ref line3 'type) "result")
  ;; Process should exit cleanly
  (claude-process-wait cp)
  (check-false (claude-process-running? cp)))

;; ─── Kill ────────────────────────────────────────────────────────────────────

(test-case "claude-process-kill terminates the process"
  ;; Use 'cat' which blocks indefinitely on stdin
  (define cp (make-claude-process
              #:command (list (find-executable-path "cat"))))
  (check-true (claude-process-running? cp))
  (claude-process-kill cp)
  (claude-process-wait cp)
  (check-false (claude-process-running? cp)))

;; ─── stdin access ────────────────────────────────────────────────────────────

(test-case "claude-process-stdin is writable"
  (define cp (make-claude-process
              #:command (list (find-executable-path "cat"))))
  (check-true (output-port? (claude-process-stdin cp)))
  (claude-process-kill cp)
  (claude-process-wait cp))

;; ─── default-claude-flags includes --verbose ────────────────────────────────

(test-case "default-claude-flags includes --verbose"
  (check-not-false (member "--verbose" default-claude-flags)
                   "--verbose must be in the default flags"))

;; ─── make-claude-command ────────────────────────────────────────────────────

(test-case "make-claude-command without custom path uses find-claude-path"
  (define cmd (make-claude-command))
  ;; First element is the claude binary path (path? or string?)
  (check-not-false (or (string? (car cmd)) (path? (car cmd))))
  ;; Flags should follow
  (check-not-false (member "--verbose" (cdr cmd))))

(test-case "make-claude-command with custom path uses it"
  (define cmd (make-claude-command #:claude-path "/custom/bin/claude"))
  (check-equal? (car cmd) "/custom/bin/claude")
  (check-not-false (member "--verbose" (cdr cmd))))
