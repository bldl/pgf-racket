#lang racket

#|

All concrete token types have an 'a' field that may be used to store
annotation(s). The semantics of any annotation value vary depending on
the processing phase.

|#

(require "util.rkt")

(abstract-struct* Token () #:transparent)

(define-syntax-rule (define-token* n fld-spec ...)
  (concrete-struct* n Token (fld-spec ...) #:transparent))

(define-token* TE a s)
(define-token* LE a)
(define-token* GBeg a)
(define-token* GEnd a)
