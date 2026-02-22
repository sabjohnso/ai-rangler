#lang racket/base

;; Session tab — composes chat-view, input-area, and status-bar.
;; A timer drains the session's event channel on the GUI thread
;; and dispatches events to the appropriate widgets.

(require racket/class
         racket/gui/base
         racket/async-channel
         "../session.rkt"
         "../event.rkt"
         "chat-view.rkt"
         "input-area.rkt"
         "status-bar.rkt")

(provide session-tab%)

(define session-tab%
  (class vertical-panel%
    (init-field session)
    (super-new)

    ;; Track whether we've seen the first assistant message
    ;; to insert the "--- Assistant ---" separator.
    (define in-assistant-turn? #f)

    ;; ─── Child widgets ───────────────────────────────────────────────────

    (define input (new input-area%
                       [parent this]
                       [on-send (λ (msg) (handle-user-send msg))]))

    (define chat (new chat-view% [parent this]))

    (define status (new status-bar% [parent this]))

    ;; ─── Send handler ────────────────────────────────────────────────────

    (define (handle-user-send msg)
      (send chat append-user-message msg)
      (session-send session msg)
      (send input set-enabled #f))

    ;; ─── Event drain timer ───────────────────────────────────────────────

    (define events-ch (session-events session))

    (define timer
      (new timer%
           [notify-callback (λ () (drain-events))]
           [interval 50]))

    (define (drain-events)
      (let loop ()
        (define evt (async-channel-try-get events-ch))
        (when evt
          (dispatch-event evt)
          (loop))))

    ;; ─── Event dispatch ──────────────────────────────────────────────────

    (define (dispatch-event evt)
      (cond
        [(event:init? evt)
         (send status set-state 'idle)]

        [(event:text-delta? evt)
         (unless in-assistant-turn?
           (send chat begin-assistant-message)
           (set! in-assistant-turn? #t))
         (send chat append-assistant-text (event:text-delta-text evt))
         (send status set-state 'working)]

        [(event:tool-start? evt)
         (send chat append-tool-notification
               (event:tool-start-tool-name evt)
               "started")
         (send status set-state 'tool-active)
         (send status set-tool-name (event:tool-start-tool-name evt))]

        [(event:tool-end? evt)
         (send status set-state 'working)
         (send status set-tool-name #f)]

        [(event:assistant-message? evt)
         ;; Complete assistant message — if we didn't stream deltas,
         ;; display the full text from content blocks.
         (unless in-assistant-turn?
           (send chat begin-assistant-message)
           (for ([block (event:assistant-message-content evt)])
             (when (and (hash? block)
                        (equal? (hash-ref block 'type #f) "text"))
               (send chat append-assistant-text (hash-ref block 'text "")))))
         ;; Reset for next turn.
         (set! in-assistant-turn? #f)
         (send status set-state 'idle)
         (send input set-enabled #t)]

        [(event:result? evt)
         (send chat append-result
               (event:result-text evt)
               (event:result-cost-usd evt))
         (send status set-state 'idle)
         (when (event:result-cost-usd evt)
           (send status set-cost (event:result-cost-usd evt)))
         (send input set-enabled #t)
         (set! in-assistant-turn? #f)]

        [(event:error? evt)
         (send chat append-error (event:error-message evt))
         (send status set-state 'error)
         (send input set-enabled #t)
         (set! in-assistant-turn? #f)]

        [else (void)]))

    ;; ─── Theme ───────────────────────────────────────────────────────────

    (define/public (apply-theme)
      (send chat apply-theme)
      (send input apply-theme))

    ;; ─── Cleanup ─────────────────────────────────────────────────────────

    (define/public (stop)
      (send timer stop)
      (session-stop session))))
