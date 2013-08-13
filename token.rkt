#lang racket

#|

All concrete token types have an 'a' field that may be used to store
annotation(s). The semantics of any annotation value vary depending on
the processing phase.

|#

(struct Token () #:transparent)
(provide Token?)

(define-syntax-rule (define-token* n fld-spec ...)
  (begin
    (struct n Token (fld-spec ...) #:transparent)
    (provide (struct-out n))))

(define-token* Text a s)
(define-token* Line a)
(define-token* GBeg a)
(define-token* GEnd a)
