#!/usr/bin/env racket
#lang racket/base

;; Mock claude-code subprocess for testing.
;; Emits canned NDJSON to stdout, reads stdin (for send tests).
;; Behaviour controlled by command-line args.

(require json
         racket/cmdline)

(define emit-stderr? (make-parameter #f))
(define stderr-lines (make-parameter 0))

(define (write-line! j)
  (write-json j (current-output-port))
  (newline)
  (flush-output))

(define (main)
  (command-line
   #:once-each
   ["--emit-stderr" n "Write N lines to stderr"
    (emit-stderr? #t)
    (stderr-lines (string->number n))]
   #:args ()
   (run)))

(define (run)
  ;; If requested, write lines to stderr
  (when (emit-stderr?)
    (for ([i (in-range (stderr-lines))])
      (fprintf (current-error-port) "stderr-line-~a\n" i))
    (flush-output (current-error-port)))

  ;; Always emit a system init message first
  (write-line! (hasheq 'type "system"
                       'subtype "init"
                       'data (hasheq 'session_id "mock-session"
                                     'tools '("Bash" "Read")
                                     'model "mock-model")))

  ;; Emit stream events to exercise working/tool-active states
  ;; 1. Text block start (→ raw, no state change)
  (write-line! (hasheq 'type "stream_event"
                       'event (hasheq 'type "content_block_start"
                                      'index 0
                                      'content_block (hasheq 'type "text"
                                                             'text ""))))
  ;; 2. Text delta (→ event:text-delta, state → working)
  (write-line! (hasheq 'type "stream_event"
                       'event (hasheq 'type "content_block_delta"
                                      'index 0
                                      'delta (hasheq 'type "text_delta"
                                                     'text "Hello "))))
  ;; 3. Content block stop for text (→ event:tool-end, state → working)
  (write-line! (hasheq 'type "stream_event"
                       'event (hasheq 'type "content_block_stop"
                                      'index 0)))
  ;; 4. Tool use block start (→ event:tool-start, state → tool-active)
  (write-line! (hasheq 'type "stream_event"
                       'event (hasheq 'type "content_block_start"
                                      'index 1
                                      'content_block (hasheq 'type "tool_use"
                                                             'id "toolu_mock"
                                                             'name "Read"
                                                             'input (hasheq)))))
  ;; 5. Content block stop for tool (→ event:tool-end, state → working)
  (write-line! (hasheq 'type "stream_event"
                       'event (hasheq 'type "content_block_stop"
                                      'index 1)))

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

(module+ main
  (main))
