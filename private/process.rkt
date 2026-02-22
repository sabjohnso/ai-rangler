#lang racket/base

;; Subprocess lifecycle for claude-code processes.
;; Handles spawning with correct flags and environment,
;; and provides clean kill/wait operations.

(require racket/system)

(provide make-claude-process
         claude-process?
         claude-process-stdin
         claude-process-stdout
         claude-process-stderr
         claude-process-running?
         claude-process-kill
         claude-process-wait
         default-claude-flags
         make-claude-command)

(struct claude-process (stdin stdout stderr handle)
  #:transparent)

;; Default flags for a claude-code session.
(define default-claude-flags
  '("--print"
    "--output-format" "stream-json"
    "--input-format" "stream-json"
    "--include-partial-messages"
    "--verbose"))

;; Resolve the claude executable to an absolute path via PATH lookup.
;; Falls back to "claude" (bare name) if not found, so the error
;; message from subprocess is clear.
(define (find-claude-path)
  (or (find-executable-path "claude")
      "claude"))

(define (make-claude-command #:claude-path [claude-path #f])
  (cons (or claude-path (find-claude-path)) default-claude-flags))

;; Build a clean environment for the child process.
;; Strips CLAUDECODE so claude-code doesn't refuse to start
;; when launched from inside another Claude Code session.
(define (make-clean-environment)
  (define env (environment-variables-copy
               (current-environment-variables)))
  (environment-variables-set! env #"CLAUDECODE" #f)
  env)

;; Spawn a claude-code subprocess (or any command for testing).
;; Returns a claude-process struct with stdin/stdout/stderr ports
;; and the subprocess handle.
;;
;; #:command — override the command+args list (for testing with mocks)
;; #:cwd    — working directory for the subprocess
(define (make-claude-process #:command [command (make-claude-command)]
                             #:cwd [cwd #f])
  (define-values (proc stdout stdin stderr)
    (parameterize ([current-environment-variables (make-clean-environment)])
      (apply subprocess #f #f #f (map (λ (s) (if (path? s) (path->string s) s))
                                      command))))
  (claude-process stdin stdout stderr proc))

;; Is the process still running?
(define (claude-process-running? cp)
  (equal? 'running (subprocess-status (claude-process-handle cp))))

;; Send SIGTERM / kill the process.
(define (claude-process-kill cp)
  (subprocess-kill (claude-process-handle cp) #t))

;; Wait for the process to exit. Returns the exit status.
(define (claude-process-wait cp)
  (subprocess-wait (claude-process-handle cp))
  (subprocess-status (claude-process-handle cp)))
