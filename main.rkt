#lang racket

#|

Implements the algorithm from Kiselyov et al: Lazy v. Yield:
Incremental, Linear Pretty-Printing (2012). With the caveat that some
of the auxiliary data structures used here do not have the required
algorithmic properties.

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

(define* page-width (make-parameter 75))

 (define-syntax-rule (matches? e pat)
   (match e (pat #t) (_ #f)))

;; TODO We require very stringent algorithmic properties for state
;; updates and queries for the overall algorithm to get the original
;; properties.
(struct GrpSt (p q) #:transparent #:mutable)

(define grp-empty (GrpSt 0 dq-empty))

(define (grp-null? grp)
  (dq-empty? (GrpSt-q grp)))

(define (GrpSt-p-q grp)
  (values (GrpSt-p grp) (GrpSt-q grp)))

;; This code is so intricate that we adapt the original fairly
;; faithfully for easier comparison. TODO Add abstraction to state
;; handling to make it easier to replace the used data structures.
(define* (make-annotate-width)
  (let ((grp grp-empty))
    (define (go grp t)
      (if (grp-null? grp)
	  (match t
	    ((TE _ _)
	     (begin (yield t) grp))
	    ((LE _)
	     (begin (yield t) grp))
	    ((GEnd _)
	     (begin (yield t) grp))
	    ((GBeg p)
	     (let ((p-w (+ p (page-width))))
	     (GrpSt p-w (dq-singleton (cons p-w dq-empty))))))
	  (match t
	    ((GBeg p)
	     (let* ((w (page-width))
		    (p0 (GrpSt-p grp))
		    (grps (GrpSt-q grp))
		    (st (GrpSt p0 (dq-push-r grps 
					     (cons (+ p w) dq-empty)))))
	       (check st p)))
	    ((GEnd p)
	     (let*-values (([p0 q] (GrpSt-p-q grp))
			   ([r q^] (dq-pop-r q))
			   ([b] (cdr r)))
	       (pop p0 q^ (dq-push-f-r b (GBeg p) (GEnd p)))))
	    ((TE p _)
	     (let*-values (([p0] (GrpSt-p grp))
			   ([q] (GrpSt-q grp)))
	       (check (GrpSt p0 (push t q)) p)))
	    ((LE p)
	     (let*-values (([p0] (GrpSt-p grp))
			   ([q] (GrpSt-q grp)))
	       (check (GrpSt p0 (push t q)) p))))))
    (define (push t q)
      (let*-values (([r q^] (dq-pop-r q))
		    ([p b] (first-rest r)))
	(dq-push-r q^ (cons p (dq-push-r b t)))))
    (define (pop p0 q b)
      (if (dq-empty? q)
	  (begin
	    (dq-each b yield)
	    grp-empty)
	  (let ()
	    (define (f r)
	      (define-values (p-r b^) (first-rest r))
	      (cons p-r (dq-append b^ b)))
	    (define n-q (dq-modify-r q f))
	    (GrpSt p0 n-q))))
    ;; GrpSt, pos -> GrpSt
    (define (check st p)
      (define p0 (GrpSt-p st))
      (define q (GrpSt-q st))
      (if (and (<= p p0)
	       ;; This condition may seem weird, and deals with an
	       ;; edge case. Assumes normalized input. (We might
	       ;; consider having a parameter to indicate whether the
	       ;; input is normalized. We could then avoid this unsafe
	       ;; optimization at the cost of some performance.)
	       (<= (dq-length q) (page-width)))
	  st ;; unchanged
	  ;; We already know the outermost group does not fit.
	  (let*-values (([f q^] (dq-pop-f q))
			([b] (cdr f)))
	    (yield (GBeg 'too-far))
	    (dq-each b yield)
	    (check^ q^ p))))
    (define (check^ q p)
      (if (dq-empty? q)
	  grp-empty
	  (let ((p^ (car (dq-peek-f q))))
	    (check (GrpSt p^ q) p))))
    (lambda (t)
      ;; (pretty-println `(read ,t st ,grp)) ;; verbose output
      (writeln `(read ,t)) ;; output similar to paper
      (set! grp (go grp t)))))

(module* main #f
  (require "sexp.rkt")
  (page-width 3)
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
    ;;'(concat "Hello" " " "world")
    ;;'(group (concat))
    ;;'(group (concat "" ""))
    ;;'(group (group "x"))
    ;;'(group (concat (group "a") "b"))
    '(group (concat "A" line (group (concat "B" line "C")))) ;; from paper
    )))
