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
(define-doc* Concat ds)
(define-doc* Group d)

(define* (cat . ds)
  (Concat ds))

(define* (doc->tseq d)
  (match d
    ((Text "") (void))
    ((Text s) (yield (TE #f s)))
    ((Line) (yield (LE #f)))
    ((Concat ds) (for-each doc->tseq ds))
    ((Group d0) (let ((d (normalize d0)))
		  (when d
		    (yield (GBeg #f))
		    (doc->tseq d)
		    (yield (GEnd #f)))))
    (_ (error "unknown document element" d))))

(define (normalize d)
  (match d
    ((Group d0) (normalize d0))
    ((Text "") #f)
    ((Concat (list)) #f)
    ((Concat (list-rest (Text "") lst)) (normalize (Concat lst)))
    (_ d)))

(module* main #f
  (for-each
   (lambda (d)
     (pretty-println d)
     ((producer-compose
       writeln
       doc->tseq
       (thunk (yield d)))))
   (list
    (cat
     (Text "Hello")
     (Text " ")
     (Text "world"))
    (Group (Concat '()))
    (Group (Concat (list (Text "") (Text ""))))
    (Group (Group (Text "x")))
    (Group (cat (Group (Text "a")) (Text "b")))
    )))
