#lang racket

(require (only-in srfi/13 string-trim-both))

(provide define* define-syntax*)

(define-syntax define*
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin
       (define (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin
       (define (name arg ...) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define name body ...)
       (provide name)))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name stx) body ...)
     (begin
       (define-syntax (name stx) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define-syntax name body ...)
       (provide name)))))

(define-syntax* define-syntax-rule*
  (syntax-rules ()
    ((_ (name rest ...) body)
     (begin
       (define-syntax-rule (name rest ...) body)
       (provide name)))))

(define-syntax-rule*
  (struct* nm rest ...)
  (begin
    (struct nm rest ...)
    (provide (struct-out nm))))

(define-syntax-rule*
  (abstract-struct* nm rest ...)
  (begin
    (struct nm rest ...)
    (provide (except-out (struct-out nm) nm))))

(define* println
  (case-lambda
    ((datum) (begin (print datum) (newline)))
    ((datum out) (begin (print datum out) (newline out)))))

(define* writeln
  (case-lambda
    ((datum) (begin (write datum) (newline)))
    ((datum out) (begin (write datum out) (newline out)))))

(define* pretty-println
  (case-lambda
    ((datum) (begin (pretty-print datum) (newline)))
    ((datum out) (begin (pretty-print datum out) (newline out)))))

(define* (printfln . args)
  (apply printf args) (newline))

(define-syntax data-for
  (syntax-rules ()
    ((_ nm ())
     (void))
    ((_ nm ((ctor fld ...) more ...))
     (begin
       (struct ctor nm (fld ...) #:transparent)
       (data-for nm (more ...))))))

(define-syntax* data
  (syntax-rules ()
    ((_ nm lst)
     (begin
       (struct nm () #:transparent)
       (data-for nm lst)))))

(define-syntax data-for*
  (syntax-rules ()
    ((_ nm ())
     (void))
    ((_ nm ((ctor fld ...) more ...))
     (begin
       (struct* ctor nm (fld ...) #:transparent)
       (data-for* nm (more ...))))))

(define-syntax* data*
  (syntax-rules ()
    ((_ nm lst)
     (begin
       (abstract-struct* nm () #:transparent)
       (data-for* nm lst)))))

(define-syntax* data/anno*
  (syntax-rules ()
    ((_ nm lst)
     (begin
       (struct nm ((anno #:auto #:mutable)) #:transparent)
       (provide (except-out (struct-out nm) nm))
       (data-for* nm lst)))))

(define-syntax* fix
  (syntax-rules ()
    ((_ fn arg ...)
     (lambda rest (apply fn arg ... rest)))))

(define* (words s)
  (regexp-split #px"\\s+" (string-trim-both s)))

(define* (force/rec d)
  (if (promise? d)
      (force/rec (force d))
      d))

(define-syntax-rule*
  (with-forced d body ...)
  (let ((d (force d))) body ...))

(define-syntax-rule*
  (shift-car empty-expr car-var cdr-var lst-var body ...)
  (if (null? lst-var)
      empty-expr
      (let ((car-var (car lst-var))
            (cdr-var (cdr lst-var)))
        body ...)))

;; Applies 'f' to all but the last member of 'lst'.
(define* (map/skip-last f lst)
  (cond
   ((null? lst) lst)
   ((null? (cdr lst)) lst)
   (else (cons (f (car lst)) (map/skip-last f (cdr lst))))))

(define* (width-divider n)
  (let* ((s-lst (for/list
            ((i (in-range 4 (+ n 1))))
            (format "~a" (modulo i 10))))
         (s (apply string-append "// " s-lst)))
    s))

(define-syntax-rule*
  (while cond action ...)
  (let continue ()
      (when cond
        action ...
        (continue))))

(define-syntax-rule*
  (loop break action ...)
  (let/ec break
    (let continue ()
      action ...
      (continue))))

(define-syntax* aif
  (syntax-rules ()
    ((_ n c t e)
     (let ((n c))
       (if n t e)))))
