#lang racket/base

;; Translate raw JSON hashes (from claude-code NDJSON) into typed event structs.
;; Pure function — no I/O, no side effects.

(require "event.rkt")

(provide json->session-event)

;; json->session-event : jsexpr string -> session-event?
;; Maps a parsed JSON hash from claude-code's stream-json output
;; to the appropriate event struct. `session-id` is the session
;; context supplied by the caller.
(define (json->session-event j session-id)
  (define now (current-inexact-milliseconds))
  (define type (hash-ref j 'type #f))
  (case type
    [("system")    (translate-system j now session-id)]
    [("assistant") (translate-assistant j now session-id)]
    [("stream_event") (translate-stream-event j now session-id)]
    [("result")    (translate-result j now session-id)]
    [else          (event:raw now session-id j)]))

;; ─── Translators ─────────────────────────────────────────────────────────────

(define (translate-system j now session-id)
  (define data (hash-ref j 'data (hasheq)))
  (event:init now
              session-id
              (hash-ref data 'session_id session-id)
              (hash-ref data 'tools '())
              (hash-ref data 'model "unknown")))

(define (translate-assistant j now session-id)
  (event:assistant-message now
                           session-id
                           (hash-ref j 'content '())
                           (hash-ref j 'model "unknown")))

(define (translate-stream-event j now session-id)
  (define evt (hash-ref j 'event (hasheq)))
  (define evt-type (hash-ref evt 'type #f))
  (case evt-type
    [("content_block_delta") (translate-delta evt now session-id)]
    [("content_block_start") (translate-block-start evt now session-id)]
    [else (event:raw now session-id j)]))

(define (translate-delta evt now session-id)
  (define delta (hash-ref evt 'delta (hasheq)))
  (define delta-type (hash-ref delta 'type #f))
  (case delta-type
    [("text_delta")
     (event:text-delta now session-id
                       (hash-ref delta 'text "")
                       (hash-ref evt 'index 0))]
    [else (event:raw now session-id evt)]))

(define (translate-block-start evt now session-id)
  (define block (hash-ref evt 'content_block (hasheq)))
  (define block-type (hash-ref block 'type #f))
  (case block-type
    [("tool_use")
     (event:tool-start now session-id
                       (hash-ref block 'id "")
                       (hash-ref block 'name "")
                       (hash-ref block 'input (hasheq)))]
    [else (event:raw now session-id (hash-set evt 'source "content_block_start"))]))

(define (translate-result j now session-id)
  (event:result now
                session-id
                (hash-ref j 'is_error #f)
                (hash-ref j 'result #f)
                (hash-ref j 'total_cost_usd #f)
                (hash-ref j 'usage (hasheq))
                (hash-ref j 'duration_ms 0)
                (hash-ref j 'num_turns 0)))
