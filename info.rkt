#lang info
(define collection "ai-rangler")
(define deps '("base" "gui-lib" "rackunit-lib" "syntax-color-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/ai-rangler.scrbl" ())))
(define pkg-desc "Manage and chat with multiple AI development tool sessions")
(define version "0.7")
(define pkg-authors '(sbj))
(define license '(Apache-2.0 OR MIT))
