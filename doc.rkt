#lang racket

#|

Support for document data structures and turning them into a token
stream. Token stream generation is done with normalization. It is
probably impossible to do with pleasing computational characteristics
afterwards, important as it may be.

|#

(require "simple-generator.rkt" "token.rkt" "util.rkt")

(abstract-struct* Doc () #:transparent)

(define-syntax-rule (define-doc* n fld-spec ...)
  (concrete-struct* n Doc (fld-spec ...) #:transparent))

(define-doc* Text s)
(define-doc* Line)
(define-doc* Concat d1 d2)
(define-doc* Group d)

(define* (doc->tseq d)
  (match d
    ((Text "") (void))
    ((Text s) (yield (TE #f s)))
    ((Line) (yield (LE #f)))
    ((Concat d1 d2) (begin (doc->tseq d1) (doc->tseq d2)))
    ((Group d0) (begin 
		  (yield (GBeg #f))
		  (let ((d (normalize d0)))
		    (when d
		      (doc->tseq d)))
		  (yield (GEnd #f))))
    (_ (error "unknown document element" d))))

(define (normalize d)
  (match d
    ((Group d0) (normalize d0))
    ((Text "") #f)
    ((Concat (Text "") d0) (normalize d0))
    (_ d)))
