#lang racket/base

;; AI Rangler — manage and chat with AI development tool sessions.
;; This module re-exports the public API and provides the CLI entry point.

(require "private/session.rkt"
         "private/event.rkt")

(provide (all-from-out "private/session.rkt")
         (all-from-out "private/event.rkt"))

(module+ test
  (require rackunit
           "private/event.rkt"
           "private/session.rkt")

  (check-true (procedure? session-send))
  (check-true (procedure? session-state))
  (check-true (procedure? session-events))
  (check-true (procedure? session-stop))
  (check-true (procedure? session-id))
  (check-true (procedure? session-info))
  (check-true (procedure? session-event?)))

(module+ main
  (require racket/class
           racket/gui/base
           racket/cmdline
           "private/config.rkt"
           "private/gui/app.rkt"
           "private/gui/theme.rkt")

  (define cwd-param (box #f))
  (define model-param (box #f))
  (define claude-path-param (box #f))
  (define theme-param (box #f))

  (command-line
   #:program "ai-rangler"
   #:once-each
   [("--cwd") dir "Working directory for claude-code sessions"
    (set-box! cwd-param dir)]
   [("--model") model "Model to use (e.g. claude-opus-4-6)"
    (set-box! model-param model)]
   [("--claude-path") path "Path to the claude executable"
    (set-box! claude-path-param path)]
   [("--theme") name "Color theme: Light or Dark"
    (set-box! theme-param name)]
   #:args ()

   ;; Load config file and merge CLI overrides
   (define cfg (merge-cli-config (load-config)
                                 #:cwd (unbox cwd-param)
                                 #:model (unbox model-param)
                                 #:claude-path (unbox claude-path-param)
                                 #:theme (unbox theme-param)))

   ;; Apply theme from config
   (when (equal? (config-ref cfg 'default-theme) "Dark")
     (current-theme dark-theme))

   (define app (new app-frame%
                    [default-cwd (config-ref cfg 'default-cwd)]
                    [default-model (config-ref cfg 'default-model)]
                    [default-claude-path (config-ref cfg 'claude-path)]))
   (send app new-session!)
   (send app show #t)))
