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
  (concrete-struct* nm rest ...)
  (begin
    (struct nm rest ...)
    (provide (struct-out nm))))

(define-syntax-rule*
  (abstract-struct* nm rest ...)
  (begin
    (struct nm rest ... #:constructor-name ctor)
    (provide (except-out (struct-out nm) ctor))))

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

(define-syntax* fix
  (syntax-rules ()
    ((_ fn arg ...)
     (lambda rest (apply fn arg ... rest)))))

(define* (first-rest lst)
  (values (car lst) (cdr lst)))

(define* (words s)
  (regexp-split #px"\\s+" (string-trim-both s)))

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

(define-for-syntax (rw-let-vars stx)
  (quasisyntax/loc stx
   (#,@(map
	(lambda (stx)
	  (syntax-case stx ()
	    ((x v)
	     (identifier? #'x)
	     #'((x) v))
	    (_ stx)))
	(syntax->list stx)))))

;; E.g.
;; (letv ((x 1) ((y z) (values 2 3))) (list x y z))
(define-syntax* (letv stx)
  (syntax-case stx ()
    ((_ ((x v) ...) b ...)
     (with-syntax ((vs (rw-let-vars #'((x v) ...))))
       #'(let-values vs b ...)))))

;; E.g.
;; (letv* ((x 1) ((y z) (values x 3))) (list x y z))
(define-syntax* (letv* stx)
  (syntax-case stx ()
    ((_ ((x v) ...) b ...)
     (with-syntax ((vs (rw-let-vars #'((x v) ...))))
       #'(let*-values vs b ...)))))

;; E.g.
;; (letrecv ((x (thunk (z))) ((y z) (values 2 (thunk 3)))) (list (x) y (z)))
(define-syntax* (letrecv stx)
  (syntax-case stx ()
    ((_ ((x v) ...) b ...)
     (with-syntax ((vs (rw-let-vars #'((x v) ...))))
       #'(letrec-values vs b ...)))))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
