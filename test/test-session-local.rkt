#lang racket/base

(require rackunit
         racket/async-channel
         racket/list
         racket/runtime-path
         "../private/session.rkt"
         "../private/event.rkt"
         "../private/session/local.rkt")

(define-runtime-path mock-claude "helpers/mock-claude.rkt")

(define racket-exe (find-executable-path "racket"))
(define mock-path (path->string mock-claude))

(define (make-test-session . args)
  (make-local-session
   #:command (cons racket-exe (cons mock-path args))
   #:session-id "test-local-1"))

;; Drain all events from a session channel until timeout.
;; Returns the events as a list.
(define (drain-events ch [timeout-sec 5])
  (let loop ([acc '()])
    (define e (sync/timeout timeout-sec ch))
    (if e
        (loop (cons e acc))
        (reverse acc))))

;; Find the first event matching a predicate in a list.
(define (find-event pred events)
  (for/first ([e (in-list events)] #:when (pred e)) e))

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

(test-case "session-events receives assistant-message"
  (define s (make-test-session))
  (define ch (session-events s))
  (define events (drain-events ch))
  (check-not-false (find-event event:assistant-message? events))
  (session-stop s))

(test-case "session-events receives result event"
  (define s (make-test-session))
  (define ch (session-events s))
  (define events (drain-events ch))
  (define e (find-event event:result? events))
  (check-not-false e)
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
  (drain-events ch)
  ;; After all events drained, mock process exits
  (sleep 0.5)
  (define st (session-state s))
  (check-true (or (equal? st 'idle) (equal? st 'stopped)))
  (session-stop s))

;; ─── stderr drain integration ───────────────────────────────────────────────

(test-case "session processes stdout normally despite stderr pressure"
  (define s (make-test-session "--emit-stderr" "100"))
  (define ch (session-events s))
  (define events (drain-events ch))
  (check-not-false (find-event event:init? events))
  (check-not-false (find-event event:assistant-message? events))
  (check-not-false (find-event event:result? events))
  (session-stop s))

;; ─── State machine integration ──────────────────────────────────────────────
;; The reader thread processes events faster than test code can consume
;; them, so we verify the event *sequence* rather than checking live
;; session-state at each step. Correct event ordering proves the state
;; machine was exercised (init → working → tool-active → working → idle).

(test-case "stream events include text-delta (exercises working state)"
  (define s (make-test-session))
  (define ch (session-events s))
  (define events (drain-events ch))
  (check-not-false (find-event event:text-delta? events)
                   "expected a text-delta event (triggers working state)")
  (session-stop s))

(test-case "stream events include tool-start (exercises tool-active state)"
  (define s (make-test-session))
  (define ch (session-events s))
  (define events (drain-events ch))
  (define e (find-event event:tool-start? events))
  (check-not-false e "expected a tool-start event (triggers tool-active state)")
  (check-equal? (event:tool-start-tool-name e) "Read")
  (session-stop s))

(test-case "stream events include tool-end after tool-start (exercises working→tool-active→working)"
  (define s (make-test-session))
  (define ch (session-events s))
  (define events (drain-events ch))
  ;; Find tool-start, then verify a tool-end follows it
  (define tool-start-idx
    (for/first ([e (in-list events)]
                [i (in-naturals)]
                #:when (event:tool-start? e))
      i))
  (check-not-false tool-start-idx "expected tool-start event")
  (define after-start (drop events (add1 tool-start-idx)))
  (check-not-false (find-event event:tool-end? after-start)
                   "expected tool-end event after tool-start")
  (session-stop s))

(test-case "full event sequence: init → stream events → assistant → result"
  (define s (make-test-session))
  (define ch (session-events s))
  (define events (drain-events ch))
  ;; Verify the overall shape: init first, result last, stream events in between
  (check-pred event:init? (car events))
  (define last-evt (car (reverse events)))
  (check-pred event:result? last-evt)
  ;; Check we got the key event types in between
  (check-true (> (length events) 4)
              "expected more than 4 events with stream events included")
  (session-stop s))
