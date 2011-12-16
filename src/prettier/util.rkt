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

(define* write-nl
  (case-lambda
    ((datum) (begin (write datum) (newline)))
    ((datum out) (begin (write datum out) (newline out)))))

(define* display-nl
  (case-lambda
    ((datum) (begin (display datum) (newline)))
    ((datum out) (begin (display datum out) (newline out)))))

(define* pretty-nl
  (case-lambda
    ((datum) (begin (pretty-print datum) (newline)))
    ((datum out) (begin (pretty-print datum out) (newline out)))))

(define-syntax* data-for
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

(define-syntax* fix
  (syntax-rules ()
    ((_ fn arg ...)
     (lambda rest (apply fn arg ... rest)))))

(define* (words s)
  (regexp-split #px"\\s+" (string-trim-both s)))
