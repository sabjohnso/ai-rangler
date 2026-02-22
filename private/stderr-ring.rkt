#lang racket/base

;; Bounded ring buffer for draining stderr from a subprocess.
;; A drain thread reads lines into the ring; on unexpected exit the
;; last N lines are available for error reporting.

(provide make-stderr-ring
         stderr-ring-push!
         stderr-ring-lines
         start-stderr-drain)

;; ─── Ring buffer struct ─────────────────────────────────────────────────────

(struct stderr-ring (vec count-box write-idx-box max-lines)
  #:mutable #:transparent)

(define (make-stderr-ring #:max-lines [max-lines 50])
  (stderr-ring (make-vector max-lines #f)
               (box 0)
               (box 0)
               max-lines))

;; Push a line into the ring, overwriting the oldest if full.
(define (stderr-ring-push! ring line)
  (define vec (stderr-ring-vec ring))
  (define idx-box (stderr-ring-write-idx-box ring))
  (define cnt-box (stderr-ring-count-box ring))
  (define max (stderr-ring-max-lines ring))
  (vector-set! vec (unbox idx-box) line)
  (set-box! idx-box (modulo (add1 (unbox idx-box)) max))
  (when (< (unbox cnt-box) max)
    (set-box! cnt-box (add1 (unbox cnt-box)))))

;; Return buffered lines oldest-first.
(define (stderr-ring-lines ring)
  (define vec (stderr-ring-vec ring))
  (define cnt (unbox (stderr-ring-count-box ring)))
  (define widx (unbox (stderr-ring-write-idx-box ring)))
  (define max (stderr-ring-max-lines ring))
  (cond
    [(zero? cnt) '()]
    [(< cnt max)
     ;; Not yet wrapped — lines are 0..cnt-1
     (for/list ([i (in-range cnt)])
       (vector-ref vec i))]
    [else
     ;; Wrapped — oldest is at write-idx, read forward
     (for/list ([i (in-range max)])
       (vector-ref vec (modulo (+ widx i) max)))]))

;; ─── Drain thread ───────────────────────────────────────────────────────────

;; Spawn a thread that reads lines from `port` and pushes them into
;; `ring` until EOF. Returns the thread handle.
(define (start-stderr-drain ring port)
  (thread
   (λ ()
     (let loop ()
       (define line (read-line port 'any))
       (cond
         [(eof-object? line) (void)]
         [else
          (stderr-ring-push! ring line)
          (loop)])))))
