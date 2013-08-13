#lang racket

#|

Implements the algorithm from Kiselyov et al: Lazy v. Yield:
Incremental, Linear Pretty-Printing (2012).

|#

(require "bankers-deque.rkt" "simple-generator.rkt" 
	 "token.rkt" "util.rkt")

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

(struct GrpSt (dq outer) #:transparent #:mutable)

(define* (make-annotate-width)
  (let ((grp #f))
    (lambda (t)
      (match t
	((GBeg _) 
	 (set! grp (GrpSt dq-empty grp)))
	((GEnd p) 
	 (begin
	   (unless grp
	     (error "group end without begin"))
	   (define b (GrpSt-dq grp))
	   (set! grp (GrpSt-outer grp))
	   (if (not grp)
	       (begin
		 (yield (GBeg p))
		 (dq-each b yield)
		 (yield t))
	       (let ((dq (dq-append (GrpSt-dq grp)
				    (dq-push-f-r b (GBeg p) t))))
		 (set-GrpSt-dq! grp dq)))))
	(_
	 (if (not grp)
	     (yield t)
	     (let ((dq (dq-push-r (GrpSt-dq grp) t)))
	       (set-GrpSt-dq! grp dq))))))))

(module* main #f
  (require "sexp.rkt")
  (for-each
   (lambda (d)
     (pretty-println d)
     ((producer-compose
       writeln
       (make-annotate-width)
       (make-annotate-position)
       sexp->tseq
       (thunk (yield d)))))
   (list
    '(concat "Hello" " " "world")
    '(group (concat))
    '(group (concat "" ""))
    '(group (group "x"))
    '(group (concat (group "a") "b"))
    '(group (concat "A" line (group (concat "B" line "C")))) ;; from paper
    )))
