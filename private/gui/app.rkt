#lang racket/base

;; Application frame — main window with tab panel and menu bar.
;; Manages multiple session tabs. "New Session" creates a new
;; local claude-code session in a new tab.

(require racket/class
         racket/gui/base
         racket/list
         "../session/local.rkt"
         "session-tab.rkt"
         "theme.rkt")

(provide app-frame%)

(define app-frame%
  (class frame%
    (init-field [default-cwd #f]
                [default-model #f]
                [default-claude-path #f])
    (super-new [label "AI Rangler"]
               [width 800]
               [height 600])

    ;; ─── Menu bar ────────────────────────────────────────────────────────

    (define mb (new menu-bar% [parent this]))
    (define file-menu (new menu% [parent mb] [label "&File"]))

    (new menu-item%
         [parent file-menu]
         [label "&New Session"]
         [shortcut #\n]
         [callback (λ (item evt) (new-session!))])

    (new separator-menu-item% [parent file-menu])

    (new menu-item%
         [parent file-menu]
         [label "&Quit"]
         [shortcut #\q]
         [callback (λ (item evt) (close-all-and-exit))])

    ;; ─── View menu with theme submenu ───────────────────────────────────

    (define view-menu (new menu% [parent mb] [label "&View"]))
    (define theme-menu (new menu% [parent view-menu] [label "Theme"]))

    (define light-item
      (new checkable-menu-item%
           [parent theme-menu]
           [label "Light"]
           [checked #t]
           [callback (λ (item evt) (switch-theme! light-theme))]))

    (define dark-item
      (new checkable-menu-item%
           [parent theme-menu]
           [label "Dark"]
           [callback (λ (item evt) (switch-theme! dark-theme))]))

    (define (switch-theme! t)
      (current-theme t)
      (send light-item check (equal? (theme-name t) "Light"))
      (send dark-item check (equal? (theme-name t) "Dark"))
      (for ([tab-pair (in-list tabs)])
        (send (cdr tab-pair) apply-theme)))

    ;; ─── Tab panel ───────────────────────────────────────────────────────

    ;; Each tab is tracked as (cons label session-tab%)
    (define tabs '())
    (define tab-counter 0)

    (define tab-panel
      (new tab-panel%
           [parent this]
           [choices '()]
           [callback (λ (tp evt) (switch-tab))]))

    ;; ─── Tab management ──────────────────────────────────────────────────

    (define/public (new-session! #:cwd [cwd default-cwd]
                                 #:model [model default-model])
      (set! tab-counter (add1 tab-counter))
      (define label (format "Session ~a" tab-counter))
      (define session (make-local-session
                       #:session-id (format "session-~a" tab-counter)
                       #:cwd cwd
                       #:claude-path default-claude-path))
      (define tab (new session-tab%
                       [parent tab-panel]
                       [session session]))
      (set! tabs (append tabs (list (cons label tab))))
      (send tab-panel append label)
      ;; Show the new tab
      (define idx (sub1 (length tabs)))
      (send tab-panel set-selection idx)
      (show-tab idx)
      tab)

    (define (switch-tab)
      (show-tab (send tab-panel get-selection)))

    (define (show-tab idx)
      (define panel-to-show (cdr (list-ref tabs idx)))
      (send tab-panel change-children
            (λ (children) (list panel-to-show))))

    ;; ─── Cleanup ─────────────────────────────────────────────────────────

    (define (close-all-and-exit)
      (for ([t (in-list tabs)])
        (send (cdr t) stop))
      (send this show #f))

    (define/augment (on-close)
      (for ([t (in-list tabs)])
        (send (cdr t) stop)))))
