#lang racket/base

(require rackunit
         racket/async-channel
         "../private/session.rkt")

;; ─── Mock implementation of gen:session ──────────────────────────────────────

(struct mock-session (id state-box events-ch)
  #:methods gen:session
  [(define (session-send s msg) (void))
   (define (session-state s) (unbox (mock-session-state-box s)))
   (define (session-events s) (mock-session-events-ch s))
   (define (session-stop s) (set-box! (mock-session-state-box s) 'stopped))
   (define (session-id s) (mock-session-id s))
   (define (session-info s) (hasheq 'type "mock"))])

(define (make-mock)
  (mock-session "mock-1" (box 'idle) (make-async-channel)))

;; ─── Interface contract tests ────────────────────────────────────────────────

(test-case "mock satisfies session? predicate"
  (check-pred session? (make-mock)))

(test-case "session-id returns the session identifier"
  (check-equal? (session-id (make-mock)) "mock-1"))

(test-case "session-state returns current state"
  (check-equal? (session-state (make-mock)) 'idle))

(test-case "session-events returns an async-channel"
  (check-true (async-channel? (session-events (make-mock)))))

(test-case "session-stop transitions to stopped"
  (define s (make-mock))
  (session-stop s)
  (check-equal? (session-state s) 'stopped))

(test-case "session-send is callable"
  (define s (make-mock))
  (check-not-exn (λ () (session-send s "hello"))))

(test-case "session-info returns metadata hash"
  (define s (make-mock))
  (define info (session-info s))
  (check-true (hash? info))
  (check-equal? (hash-ref info 'type) "mock"))
