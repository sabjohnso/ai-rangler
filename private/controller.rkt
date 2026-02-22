#lang racket/base

;; Controller — maps session events to presentation commands.
;; Pure bridge between gen:session and gen:presenter.
;; No GUI imports; any presenter implementation works.

(require racket/async-channel
         "session.rkt"
         "event.rkt"
         "presenter.rkt"
         "fence-tracker.rkt"
         "table-tracker.rkt")

(provide make-controller
         controller-drain!
         controller-send!)

;; ─── Controller struct ──────────────────────────────────────────────────────

(struct controller (session presenter events-ch in-turn-box
                            fence-state-box table-state-box)
  #:transparent)

(define (make-controller session presenter)
  (controller session
              presenter
              (session-events session)
              (box #f)
              (box fence-state-init)
              (box table-state-init)))

;; ─── Send a user message ────────────────────────────────────────────────────

(define (controller-send! ctrl msg)
  (present! (controller-presenter ctrl) (cmd:show-user-message msg))
  (present! (controller-presenter ctrl) (cmd:set-input-enabled #f))
  (session-send (controller-session ctrl) msg))

;; ─── Drain all pending events ───────────────────────────────────────────────

(define (controller-drain! ctrl)
  (let loop ()
    (define evt (async-channel-try-get (controller-events-ch ctrl)))
    (when evt
      (dispatch-event ctrl evt)
      (loop))))

;; ─── Segment → command dispatch ───────────────────────────────────────────────

;; Emit table-tracked presenter commands for a prose segment.
(define (emit-prose! p ts-box text)
  (define-values (tsegs new-tstate) (table-track (unbox ts-box) text))
  (set-box! ts-box new-tstate)
  (for ([seg (in-list tsegs)])
    (define tag (car seg))
    (define txt (cdr seg))
    (case tag
      [(prose)       (present! p (cmd:append-assistant-text txt))]
      [(table-start) (present! p (cmd:begin-table txt))]
      [(table-row)   (present! p (cmd:append-table-row txt))]
      [(table-end)   (present! p (cmd:end-table))])))

;; Emit presenter commands for a list of tagged segments from fence-track/flush.
(define (emit-segments! ctrl segments)
  (define p (controller-presenter ctrl))
  (define ts-box (controller-table-state-box ctrl))
  (for ([seg (in-list segments)])
    (define tag (car seg))
    (define text (cdr seg))
    (case tag
      [(prose)       (emit-prose! p ts-box text)]
      [(code)        (present! p (cmd:append-code-text text))]
      [(fence-open)  (present! p (cmd:begin-code-block text))]
      [(fence-close) (present! p (cmd:end-code-block))])))

;; Flush fence state and emit any pending segments.
(define (flush-fence! ctrl)
  (define fs-box (controller-fence-state-box ctrl))
  (define-values (segs new-state) (fence-flush (unbox fs-box)))
  (set-box! fs-box new-state)
  (emit-segments! ctrl segs))

;; Flush table state and emit any pending segments.
(define (flush-table! ctrl)
  (define ts-box (controller-table-state-box ctrl))
  (define-values (tsegs new-state) (table-flush (unbox ts-box)))
  (set-box! ts-box new-state)
  (define p (controller-presenter ctrl))
  (for ([seg (in-list tsegs)])
    (define tag (car seg))
    (define txt (cdr seg))
    (case tag
      [(prose)       (present! p (cmd:append-assistant-text txt))]
      [(table-start) (present! p (cmd:begin-table txt))]
      [(table-row)   (present! p (cmd:append-table-row txt))]
      [(table-end)   (present! p (cmd:end-table))])))

;; ─── Event dispatch ─────────────────────────────────────────────────────────

(define (dispatch-event ctrl evt)
  (define p (controller-presenter ctrl))
  (define in-turn? (unbox (controller-in-turn-box ctrl)))

  (cond
    [(event:init? evt)
     (present! p (cmd:set-state 'idle))]

    [(event:text-delta? evt)
     (unless in-turn?
       (present! p (cmd:begin-assistant-message))
       (set-box! (controller-in-turn-box ctrl) #t))
     ;; Pass text through fence tracker
     (define fs-box (controller-fence-state-box ctrl))
     (define-values (segs new-state)
       (fence-track (unbox fs-box) (event:text-delta-text evt)))
     (set-box! fs-box new-state)
     (emit-segments! ctrl segs)
     (present! p (cmd:set-state 'working))]

    [(event:tool-start? evt)
     (present! p (cmd:show-tool-notification
                  (event:tool-start-tool-name evt) "started"))
     (present! p (cmd:set-state 'tool-active))
     (present! p (cmd:set-tool-name (event:tool-start-tool-name evt)))]

    [(event:tool-end? evt)
     (present! p (cmd:set-state 'working))
     (present! p (cmd:set-tool-name #f))]

    [(event:assistant-message? evt)
     (unless in-turn?
       (present! p (cmd:begin-assistant-message))
       (for ([block (event:assistant-message-content evt)])
         (when (and (hash? block)
                    (equal? (hash-ref block 'type #f) "text"))
           ;; Process through fence tracker for code blocks
           (define fs-box (controller-fence-state-box ctrl))
           (define-values (segs new-state)
             (fence-track (unbox fs-box) (hash-ref block 'text "")))
           (set-box! fs-box new-state)
           (emit-segments! ctrl segs))))
     ;; Flush any pending fence/table buffers before resetting state.
     (flush-fence! ctrl)
     (flush-table! ctrl)
     (set-box! (controller-in-turn-box ctrl) #f)
     (set-box! (controller-fence-state-box ctrl) fence-state-init)
     (set-box! (controller-table-state-box ctrl) table-state-init)
     (present! p (cmd:set-state 'idle))
     (present! p (cmd:set-input-enabled #t))]

    [(event:result? evt)
     (flush-fence! ctrl)
     (flush-table! ctrl)
     (present! p (cmd:show-result (event:result-text evt)
                                  (event:result-cost-usd evt)))
     (present! p (cmd:set-state 'idle))
     (when (event:result-cost-usd evt)
       (present! p (cmd:set-cost (event:result-cost-usd evt))))
     (present! p (cmd:set-input-enabled #t))
     (set-box! (controller-in-turn-box ctrl) #f)
     (set-box! (controller-fence-state-box ctrl) fence-state-init)
     (set-box! (controller-table-state-box ctrl) table-state-init)]

    [(event:error? evt)
     (flush-fence! ctrl)
     (flush-table! ctrl)
     (present! p (cmd:show-error (event:error-message evt)))
     (present! p (cmd:set-state 'error))
     (present! p (cmd:set-input-enabled #t))
     (set-box! (controller-in-turn-box ctrl) #f)
     (set-box! (controller-fence-state-box ctrl) fence-state-init)
     (set-box! (controller-table-state-box ctrl) table-state-init)]

    [else (void)]))
