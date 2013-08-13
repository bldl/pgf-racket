#lang racket

#|

Support for sexp-encoded document data structures and turning them
into a token stream. Token stream generation is done with
normalization. It is probably impossible to do with pleasing
computational characteristics afterwards, important as it may be.

The implementation is naturally almost the same as for "doc.rkt",
since in both cases we are translating a tree into the same kind of
output. We could translate between tree formats instead, but would
then lose incrementality.

|#

(require "simple-generator.rkt" "token.rkt" "util.rkt")

(define* (sexp->tseq d)
  (match d
    ("" (void))
    ((? string? s) (yield (TE #f s)))
    ('line (yield (LE #f)))
    ((list-rest 'concat ds) (for-each sexp->tseq ds))
    ((list 'group d0) (let ((d (normalize d0)))
			(when d
			  (yield (GBeg #f))
			  (sexp->tseq d)
			  (yield (GEnd #f)))))
    (_ (error "unknown document element" d))))

(define (normalize d)
  (match d
    ((list 'group d0) (normalize d0))
    ("" #f)
    ((list 'concat) #f)
    ((list-rest 'concat "" lst) (normalize `(concat ,@lst)))
    (_ d)))

(module* main #f
  (for-each
   (lambda (d)
     (pretty-println d)
     ((producer-compose
       writeln
       sexp->tseq
       (thunk (yield d)))))
   (list
    '(concat "Hello" " " "world")
    '(group (concat))
    '(group (concat "" ""))
    '(group (group "x"))
    '(group (concat (group "a") "b"))
    )))
