#lang racket

;;; 
;;; This module makes it relatively easy to define lazy abstract data
;;; types that follow the "even" style, as described in the paper "How
;;; to add laziness to a strict language without even being odd" by
;;; Wadler et al.
;;; 

#|
As an example, the macro invocation

  (lazy-data DOC ((NIL) 
                  (NEST n (susp doc))))

would expand to

  (begin
    (struct DOC_ () #:transparent)
    (define (DOC? x) (DOC_? (force x)))
    (struct NIL_ DOC_ () #:transparent)
    (define-syntax-rule (NIL) (lazy (NIL_)))
    (struct NEST_ DOC_ (n doc) #:transparent)
    (define-syntax-rule (NEST n doc) (lazy (NEST_ n (lazy doc)))))
|#

(require "util.rkt")
(require (for-syntax racket/syntax))

(define-syntax (lazy-data-ctors stx)
  (syntax-case stx ()
    ((_ _ ())
     #'(void))
    ((_ (adt adt_) (((ctor ctor_ ctor? ctor_?) fld ...) more ...))
     (let* ((a (syntax (fld ...)))
            (b (syntax->list a))
            (fld-names (map (lambda (stx)
                              (syntax-case stx (susp)
                                ((susp fld) #'fld)
                                (fld #'fld))) b))
            (ctor-args (map (lambda (stx)
                              (syntax-case stx (susp)
                                ((susp fld) #'(lazy fld))
                                (fld #'fld))) b)))
       #`(begin
           (struct ctor_ adt_ (#,@fld-names) #:transparent)
           (define (ctor? x) (ctor_? (force x)))
           (define-syntax-rule (ctor #,@fld-names)
             (lazy (ctor_ #,@ctor-args)))
           (lazy-data-ctors (adt adt_) (more ...))
           )))))

(define-syntax lazy-data-sub
  (syntax-rules ()
    ((_ (adt adt_ adt? adt_?) lst)
     (begin
       (struct adt_ () #:transparent)
       (define (adt? x) (adt_? (force x)))
       (lazy-data-ctors (adt adt_) lst)
       ))))

;; We retrict introductions of new identifiers into this top-level
;; macro, where we have a reference to the correct lexical context
;; into which the identifiers belong.
(define-syntax* (lazy-data stx)
  (define (mk_ n)
    (format-id stx "~a_" n))
  (define (mk? n)
    (format-id stx "~a?" n))
  (define (mk_? n)
    (format-id stx "~a_?" n))
  (syntax-case stx ()
    ((_ adt lst)
     (let* ((a (syntax->list #'lst))
            (nlst (map
                   (lambda (x)
                     (define b (syntax->list x))
                     (define c (car b))
                     (define d (cdr b))
                     #`((#,c #,(mk_ c) #,(mk? c) #,(mk_? c)) #,@d))
                   a))
            (adt^ #'adt))
       #`(lazy-data-sub (adt #,(mk_ adt^)
                             #,(mk? adt^)
                             #,(mk_? adt^))
                        (#,@nlst))))))
