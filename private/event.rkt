#lang racket/base

;; Session event types — the data contract between session implementations
;; and the GUI layer. All events share a common base (timestamp, session-id)
;; and are distinguished by struct subtyping.

(provide (struct-out session-event)
         (struct-out event:init)
         (struct-out event:text)
         (struct-out event:text-delta)
         (struct-out event:tool-start)
         (struct-out event:tool-end)
         (struct-out event:assistant-message)
         (struct-out event:result)
         (struct-out event:error)
         (struct-out event:raw))

;; Base event — every session event carries a timestamp and session-id.
(struct session-event (timestamp session-id) #:transparent)

;; Session initialization: tools available, model in use.
(struct event:init session-event (init-session-id tools model) #:transparent)

;; Complete text content block from an assistant message.
(struct event:text session-event (content) #:transparent)

;; Incremental text delta from streaming.
(struct event:text-delta session-event (text index) #:transparent)

;; A tool invocation has started.
(struct event:tool-start session-event (tool-use-id tool-name input) #:transparent)

;; A tool invocation has completed.
(struct event:tool-end session-event (tool-use-id) #:transparent)

;; Complete assistant message with all content blocks.
(struct event:assistant-message session-event (content model) #:transparent)

;; Final result when the session completes a turn.
(struct event:result session-event
  (is-error? text cost-usd usage duration-ms num-turns)
  #:transparent)

;; An error occurred.
(struct event:error session-event (message) #:transparent)

;; Fallback for unrecognized JSON messages.
(struct event:raw session-event (json) #:transparent)
