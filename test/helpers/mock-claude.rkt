#!/usr/bin/env racket
#lang racket/base

;; Mock claude-code subprocess for testing.
;; Emits canned NDJSON to stdout, reads stdin (for send tests).
;; Behaviour controlled by command-line args.

(require json)

(define (write-line! j)
  (write-json j (current-output-port))
  (newline)
  (flush-output))

(define (main)
  ;; Always emit a system init message first
  (write-line! (hasheq 'type "system"
                       'subtype "init"
                       'data (hasheq 'session_id "mock-session"
                                     'tools '("Bash" "Read")
                                     'model "mock-model")))

  ;; Emit a simple assistant message
  (write-line! (hasheq 'type "assistant"
                       'model "mock-model"
                       'content (list (hasheq 'type "text"
                                              'text "Hello from mock!"))))

  ;; Emit a result
  (write-line! (hasheq 'type "result"
                       'subtype "success"
                       'is_error #f
                       'result "Hello from mock!"
                       'total_cost_usd 0.001
                       'usage (hasheq 'input_tokens 10 'output_tokens 5)
                       'duration_ms 100
                       'num_turns 1
                       'session_id "mock-session"))

  ;; Exit cleanly
  (void))

(main)
