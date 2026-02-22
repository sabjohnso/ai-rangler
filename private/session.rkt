#lang racket/base

;; gen:session — the central abstraction for AI tool sessions.
;; The GUI programs against this interface; local and remote
;; implementations are swappable.

(require racket/generic)

(provide gen:session
         session?
         session-send
         session-state
         session-events
         session-stop
         session-id
         session-info)

(define-generics session
  ;; Send a user message (string) to the session.
  (session-send session message)
  ;; Return the current state symbol: 'init, 'idle, 'working, 'tool-active, 'stopped, 'error.
  (session-state session)
  ;; Return the async-channel carrying session-event structs.
  (session-events session)
  ;; Stop the session gracefully.
  (session-stop session)
  ;; Return the session's unique identifier (string).
  (session-id session)
  ;; Return a hash of session metadata (type, model, cwd, etc.).
  (session-info session))
