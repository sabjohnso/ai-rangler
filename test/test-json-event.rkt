#lang racket/base

(require rackunit
         "../private/event.rkt"
         "../private/json-event.rkt")

;; ─── system/init message ─────────────────────────────────────────────────────

(test-case "system init → event:init"
  (define j (hasheq 'type "system"
                    'subtype "init"
                    'data (hasheq 'session_id "sess-abc"
                                  'tools '("Bash" "Read" "Edit")
                                  'model "claude-opus-4-6")))
  (define e (json->session-event j "sess-abc"))
  (check-pred event:init? e)
  (check-equal? (event:init-init-session-id e) "sess-abc")
  (check-equal? (event:init-tools e) '("Bash" "Read" "Edit"))
  (check-equal? (event:init-model e) "claude-opus-4-6"))

;; ─── assistant message ───────────────────────────────────────────────────────

(test-case "assistant message → event:assistant-message"
  (define j (hasheq 'type "assistant"
                    'model "claude-opus-4-6"
                    'content (list (hasheq 'type "text" 'text "Hello"))))
  (define e (json->session-event j "s1"))
  (check-pred event:assistant-message? e)
  (check-equal? (event:assistant-message-model e) "claude-opus-4-6")
  (check-equal? (length (event:assistant-message-content e)) 1))

;; ─── stream_event: text_delta ────────────────────────────────────────────────

(test-case "stream_event text_delta → event:text-delta"
  (define j (hasheq 'type "stream_event"
                    'event (hasheq 'type "content_block_delta"
                                   'index 0
                                   'delta (hasheq 'type "text_delta"
                                                   'text "Hello "))))
  (define e (json->session-event j "s1"))
  (check-pred event:text-delta? e)
  (check-equal? (event:text-delta-text e) "Hello ")
  (check-equal? (event:text-delta-index e) 0))

;; ─── stream_event: content_block_start tool_use ──────────────────────────────

(test-case "stream_event content_block_start tool_use → event:tool-start"
  (define j (hasheq 'type "stream_event"
                    'event (hasheq 'type "content_block_start"
                                   'index 1
                                   'content_block (hasheq 'type "tool_use"
                                                          'id "toolu_01"
                                                          'name "Read"
                                                          'input (hasheq)))))
  (define e (json->session-event j "s1"))
  (check-pred event:tool-start? e)
  (check-equal? (event:tool-start-tool-name e) "Read")
  (check-equal? (event:tool-start-tool-use-id e) "toolu_01"))

;; ─── stream_event: content_block_start text → raw ───────────────────────────

(test-case "stream_event content_block_start text → no tool event"
  (define j (hasheq 'type "stream_event"
                    'event (hasheq 'type "content_block_start"
                                   'index 0
                                   'content_block (hasheq 'type "text"
                                                          'text ""))))
  ;; text block starts are not interesting as tool events
  (define e (json->session-event j "s1"))
  (check-pred event:raw? e))

;; ─── stream_event: content_block_stop ────────────────────────────────────────

(test-case "stream_event content_block_stop → event:tool-end"
  (define j (hasheq 'type "stream_event"
                    'event (hasheq 'type "content_block_stop"
                                   'index 1)))
  (define e (json->session-event j "s1"))
  (check-pred event:tool-end? e)
  (check-equal? (event:tool-end-tool-use-id e) "1"))

;; ─── result message ──────────────────────────────────────────────────────────

(test-case "result success → event:result"
  (define j (hasheq 'type "result"
                    'subtype "success"
                    'is_error #f
                    'result "All done."
                    'total_cost_usd 0.015
                    'usage (hasheq 'input_tokens 100 'output_tokens 50)
                    'duration_ms 5000
                    'num_turns 2
                    'session_id "sess-abc"))
  (define e (json->session-event j "sess-abc"))
  (check-pred event:result? e)
  (check-false (event:result-is-error? e))
  (check-equal? (event:result-text e) "All done.")
  (check-equal? (event:result-cost-usd e) 0.015)
  (check-equal? (event:result-num-turns e) 2))

(test-case "result error → event:result with is-error?"
  (define j (hasheq 'type "result"
                    'subtype "error"
                    'is_error #t
                    'result "Failed"
                    'total_cost_usd #f
                    'usage (hasheq)
                    'duration_ms 1000
                    'num_turns 0
                    'session_id "s1"))
  (define e (json->session-event j "s1"))
  (check-pred event:result? e)
  (check-true (event:result-is-error? e)))

;; ─── user message → event:raw ────────────────────────────────────────────────

(test-case "user message → event:raw (pass-through)"
  (define j (hasheq 'type "user"
                    'content (list (hasheq 'type "tool_result"
                                          'tool_use_id "toolu_01"
                                          'content "file data"))))
  (define e (json->session-event j "s1"))
  (check-pred event:raw? e))

;; ─── unknown type → event:raw ────────────────────────────────────────────────

(test-case "completely unknown type → event:raw"
  (define j (hasheq 'type "something_new" 'data 42))
  (define e (json->session-event j "s1"))
  (check-pred event:raw? e)
  (check-equal? (event:raw-json e) j))
