#lang racket

#|

Implements the algorithm from Kiselyov et al: Lazy v. Yield:
Incremental, Linear Pretty-Printing (2012).

|#

(require "simple-generator.rkt" "token.rkt" "util.rkt")

;; Since this generator has state, we must create a new instance for
;; every pipeline instance. Hence this generator constructor.
(define* (make-annotate-position)
  (let ((pos 0))
    (lambda (t)
      (match t
	((TE _ s) (begin
		    (set! pos (+ pos (string-length s)))
		    (yield (TE pos s))))
	((LE _) (begin
		  (set! pos (+ pos 1))
		  (yield (LE pos))))
	((GBeg _) (yield (GBeg pos)))
	((GEnd _) (yield (GEnd pos)))
	(_ (error "unknown token" t))))))

(module* main #f
  (require "sexp.rkt")
  (for-each
   (lambda (d)
     (pretty-println d)
     ((producer-compose
       writeln
       (make-annotate-position)
       sexp->tseq
       (thunk (yield d)))))
   (list
    '(concat "Hello" " " "world")
    '(group (concat))
    '(group (concat "" ""))
    '(group (group "x"))
    '(group (concat (group "a") "b"))
    )))
