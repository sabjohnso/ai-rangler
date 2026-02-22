#lang racket/base

(require rackunit
         "../private/event.rkt")

;; ─── Construction and field access ───────────────────────────────────────────

(test-case "event:init carries session-id, tools, and model"
  (define e (event:init 1000 "sess-1" "sess-1" '("Bash" "Read") "claude-opus-4-6"))
  (check-equal? (session-event-timestamp e) 1000)
  (check-equal? (session-event-session-id e) "sess-1")
  (check-equal? (event:init-tools e) '("Bash" "Read"))
  (check-equal? (event:init-model e) "claude-opus-4-6"))

(test-case "event:text carries full text content"
  (define e (event:text 2000 "sess-1" "Hello world"))
  (check-equal? (event:text-content e) "Hello world"))

(test-case "event:text-delta carries incremental text"
  (define e (event:text-delta 2001 "sess-1" "wor" 0))
  (check-equal? (event:text-delta-text e) "wor")
  (check-equal? (event:text-delta-index e) 0))

(test-case "event:tool-start carries tool name and id"
  (define e (event:tool-start 3000 "sess-1" "toolu_01" "Read" (hasheq 'file_path "/tmp/x")))
  (check-equal? (event:tool-start-tool-name e) "Read")
  (check-equal? (event:tool-start-tool-use-id e) "toolu_01")
  (check-equal? (event:tool-start-input e) (hasheq 'file_path "/tmp/x")))

(test-case "event:tool-end carries tool-use-id"
  (define e (event:tool-end 3100 "sess-1" "toolu_01"))
  (check-equal? (event:tool-end-tool-use-id e) "toolu_01"))

(test-case "event:assistant-message carries content blocks"
  (define blocks (list (hasheq 'type "text" 'text "Hi")))
  (define e (event:assistant-message 4000 "sess-1" blocks "claude-opus-4-6"))
  (check-equal? (event:assistant-message-content e) blocks)
  (check-equal? (event:assistant-message-model e) "claude-opus-4-6"))

(test-case "event:result carries final outcome"
  (define e (event:result 9000 "sess-1" #f "Done." 0.015
                          (hasheq 'input_tokens 100 'output_tokens 50)
                          5000 2))
  (check-false (event:result-is-error? e))
  (check-equal? (event:result-text e) "Done.")
  (check-equal? (event:result-cost-usd e) 0.015)
  (check-equal? (event:result-duration-ms e) 5000)
  (check-equal? (event:result-num-turns e) 2))

(test-case "event:error carries message"
  (define e (event:error 9500 "sess-1" "something broke"))
  (check-equal? (event:error-message e) "something broke"))

(test-case "event:raw wraps unknown JSON"
  (define h (hasheq 'type "unknown_thing" 'data 42))
  (define e (event:raw 9900 "sess-1" h))
  (check-equal? (event:raw-json e) h))

;; ─── Predicate dispatch ──────────────────────────────────────────────────────

(test-case "all event structs satisfy session-event?"
  (check-pred session-event? (event:init 0 "s" "s" '() "m"))
  (check-pred session-event? (event:text 0 "s" "t"))
  (check-pred session-event? (event:text-delta 0 "s" "d" 0))
  (check-pred session-event? (event:tool-start 0 "s" "id" "T" (hasheq)))
  (check-pred session-event? (event:tool-end 0 "s" "id"))
  (check-pred session-event? (event:assistant-message 0 "s" '() "m"))
  (check-pred session-event? (event:result 0 "s" #f "" 0 (hasheq) 0 0))
  (check-pred session-event? (event:error 0 "s" "err"))
  (check-pred session-event? (event:raw 0 "s" (hasheq))))

(test-case "specific predicates distinguish types"
  (check-pred event:init? (event:init 0 "s" "s" '() "m"))
  (check-false (event:text? (event:init 0 "s" "s" '() "m")))
  (check-pred event:text? (event:text 0 "s" "t"))
  (check-pred event:text-delta? (event:text-delta 0 "s" "d" 0))
  (check-pred event:tool-start? (event:tool-start 0 "s" "id" "T" (hasheq)))
  (check-pred event:tool-end? (event:tool-end 0 "s" "id"))
  (check-pred event:result? (event:result 0 "s" #f "" 0 (hasheq) 0 0))
  (check-pred event:error? (event:error 0 "s" "err"))
  (check-pred event:raw? (event:raw 0 "s" (hasheq))))
