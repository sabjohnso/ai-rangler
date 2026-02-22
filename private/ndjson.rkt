#lang racket/base

;; NDJSON (Newline-Delimited JSON) read/write utilities.
;; Each line is an independent JSON value terminated by newline.

(require json
         racket/async-channel)

(provide read-ndjson-line
         write-ndjson-line
         ndjson-port-reader)

;; Read one NDJSON line from port.
;; Returns: parsed jsexpr (hash), eof, or #f (malformed).
;; Skips blank lines.
(define (read-ndjson-line in)
  (let loop ()
    (define line (read-line in 'any))
    (cond
      [(eof-object? line) eof]
      [(equal? line "") (loop)]
      [else
       (with-handlers ([exn:fail? (λ (_) #f)])
         (read-json (open-input-string line)))])))

;; Write a jsexpr as a single JSON line to port.
(define (write-ndjson-line jsexpr out)
  (write-json jsexpr out)
  (newline out)
  (flush-output out))

;; Spawn a thread that reads NDJSON lines from `in` and puts parsed
;; objects into `ch`. Malformed lines are silently skipped.
;; Returns the thread handle.
(define (ndjson-port-reader in ch)
  (thread
   (λ ()
     (let loop ()
       (define v (read-ndjson-line in))
       (cond
         [(eof-object? v) (void)]
         [(not v) (loop)]            ; malformed — skip
         [else
          (async-channel-put ch v)
          (loop)])))))
