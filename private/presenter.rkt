#lang racket/base

;; gen:presenter — the presentation interface for AI session UIs.
;; Each UI (GUI, terminal, web) implements present! to handle
;; command structs emitted by the controller.

(require racket/generic)

(provide gen:presenter
         presenter?
         present!
         ;; Command structs
         (struct-out cmd:show-user-message)
         (struct-out cmd:begin-assistant-message)
         (struct-out cmd:append-assistant-text)
         (struct-out cmd:show-tool-notification)
         (struct-out cmd:show-error)
         (struct-out cmd:show-result)
         (struct-out cmd:set-state)
         (struct-out cmd:set-tool-name)
         (struct-out cmd:set-cost)
         (struct-out cmd:set-input-enabled))

(define-generics presenter
  (present! presenter cmd))

;; ─── Command structs ────────────────────────────────────────────────────────

(struct cmd:show-user-message (text) #:transparent)
(struct cmd:begin-assistant-message () #:transparent)
(struct cmd:append-assistant-text (text) #:transparent)
(struct cmd:show-tool-notification (tool-name action) #:transparent)
(struct cmd:show-error (message) #:transparent)
(struct cmd:show-result (text cost-usd) #:transparent)
(struct cmd:set-state (state) #:transparent)
(struct cmd:set-tool-name (name) #:transparent)
(struct cmd:set-cost (cost-usd) #:transparent)
(struct cmd:set-input-enabled (enabled?) #:transparent)
