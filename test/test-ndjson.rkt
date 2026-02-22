#lang racket/base

(require rackunit
         json
         racket/port
         racket/async-channel
         "../private/ndjson.rkt")

;; ─── read-ndjson-line ────────────────────────────────────────────────────────

(test-case "read-ndjson-line parses a valid JSON object"
  (define in (open-input-string "{\"type\":\"system\",\"data\":42}\n"))
  (define result (read-ndjson-line in))
  (check-equal? (hash-ref result 'type) "system")
  (check-equal? (hash-ref result 'data) 42))

(test-case "read-ndjson-line returns eof at end of port"
  (define in (open-input-string ""))
  (check-equal? (read-ndjson-line in) eof))

(test-case "read-ndjson-line skips blank lines"
  (define in (open-input-string "\n\n{\"x\":1}\n"))
  (check-equal? (hash-ref (read-ndjson-line in) 'x) 1))

(test-case "read-ndjson-line returns #f for malformed JSON"
  (define in (open-input-string "not-json\n"))
  (check-false (read-ndjson-line in)))

;; ─── write-ndjson-line ───────────────────────────────────────────────────────

(test-case "write-ndjson-line writes JSON followed by newline"
  (define out (open-output-string))
  (write-ndjson-line (hasheq 'type "user" 'message "hi") out)
  (define written (get-output-string out))
  ;; Must end with newline
  (check-regexp-match #rx"\n$" written)
  ;; Must be valid JSON when stripped
  (define parsed (read-json (open-input-string written)))
  (check-equal? (hash-ref parsed 'type) "user")
  (check-equal? (hash-ref parsed 'message) "hi"))

;; ─── Round-trip property ─────────────────────────────────────────────────────

(test-case "write then read round-trips"
  (define data (hasheq 'type "test" 'nums '(1 2 3) 'nested (hasheq 'a #t)))
  (define pipe-out (open-output-string))
  (write-ndjson-line data pipe-out)
  (define pipe-in (open-input-string (get-output-string pipe-out)))
  (define result (read-ndjson-line pipe-in))
  (check-equal? (hash-ref result 'type) "test")
  (check-equal? (hash-ref result 'nums) '(1 2 3))
  (check-equal? (hash-ref (hash-ref result 'nested) 'a) #t))

;; ─── ndjson-port-reader ──────────────────────────────────────────────────────

(test-case "ndjson-port-reader delivers parsed objects to channel"
  (define in (open-input-string
              (string-append "{\"n\":1}\n"
                             "{\"n\":2}\n"
                             "{\"n\":3}\n")))
  (define ch (make-async-channel))
  (define thd (ndjson-port-reader in ch))
  ;; Read all three
  (check-equal? (hash-ref (async-channel-get ch) 'n) 1)
  (check-equal? (hash-ref (async-channel-get ch) 'n) 2)
  (check-equal? (hash-ref (async-channel-get ch) 'n) 3)
  ;; After EOF the thread should finish
  (thread-wait thd)
  (check-false (thread-running? thd)))

(test-case "ndjson-port-reader skips malformed lines"
  (define in (open-input-string
              (string-append "{\"ok\":1}\n"
                             "GARBAGE\n"
                             "{\"ok\":2}\n")))
  (define ch (make-async-channel))
  (define thd (ndjson-port-reader in ch))
  (check-equal? (hash-ref (async-channel-get ch) 'ok) 1)
  (check-equal? (hash-ref (async-channel-get ch) 'ok) 2)
  (thread-wait thd))
