#lang racket/base

(require rackunit
         racket/async-channel
         racket/runtime-path
         "../private/session.rkt"
         "../private/event.rkt"
         "../private/session/local.rkt")

(define-runtime-path mock-claude "helpers/mock-claude.rkt")

(define (make-test-session)
  (make-local-session
   #:command (list (find-executable-path "racket") (path->string mock-claude))
   #:session-id "test-local-1"))

;; ─── Integration tests ──────────────────────────────────────────────────────

(test-case "make-local-session returns a session? value"
  (define s (make-test-session))
  (check-pred session? s)
  (session-stop s))

(test-case "session-id returns the provided id"
  (define s (make-test-session))
  (check-equal? (session-id s) "test-local-1")
  (session-stop s))

(test-case "session-events channel receives init event first"
  (define s (make-test-session))
  (define ch (session-events s))
  (define e (sync/timeout 5 ch))
  (check-pred event:init? e)
  (check-equal? (event:init-model e) "mock-model")
  (session-stop s))

(test-case "session-events receives assistant-message after init"
  (define s (make-test-session))
  (define ch (session-events s))
  ;; skip init
  (sync/timeout 5 ch)
  (define e (sync/timeout 5 ch))
  (check-pred event:assistant-message? e)
  (session-stop s))

(test-case "session-events receives result event"
  (define s (make-test-session))
  (define ch (session-events s))
  ;; skip init and assistant
  (sync/timeout 5 ch)
  (sync/timeout 5 ch)
  (define e (sync/timeout 5 ch))
  (check-pred event:result? e)
  (check-false (event:result-is-error? e))
  (check-equal? (event:result-text e) "Hello from mock!")
  (session-stop s))

(test-case "session-stop terminates the process"
  (define s (make-test-session))
  (session-stop s)
  ;; Give it a moment to clean up
  (sleep 0.2)
  (check-equal? (session-state s) 'stopped))

(test-case "session-info returns metadata hash"
  (define s (make-test-session))
  (define info (session-info s))
  (check-true (hash? info))
  (check-equal? (hash-ref info 'type) "local")
  (session-stop s))

(test-case "state transitions from init through idle to stopped"
  (define s (make-test-session))
  (define ch (session-events s))
  ;; Drain all events (mock emits 3 messages then exits)
  (sync/timeout 5 ch) ; init
  (sync/timeout 5 ch) ; assistant
  (sync/timeout 5 ch) ; result
  ;; After result, mock process exits — state should go to idle then stopped
  (sleep 0.5)
  ;; The mock exits, so the state should be stopped or idle
  (define st (session-state s))
  (check-true (or (equal? st 'idle) (equal? st 'stopped)))
  (session-stop s))
