#lang racket

#|

Simple generators, with the same underlying idea as described by Oleg
Kiselyov et al. http://okmij.org/ftp/continuations/PPYield/

In this implementation generators are just functions (or other
applicables) that may invoke 'yield' to output generated values. The
'yield' target must be bound dynamically to 'yield-handler'. The
binding is done automatically when you use 'producer-compose' or
'producer-chain'. Otherwise you may specify the target consumer for a
given dynamic scope using 'with-consumer'.

If your generators require state, you may use a closure, or a struct
with the prop:procedure property.

|#

(provide yield-handler
	 yield
	 with-consumer
	 producer-compose
	 producer-chain)

(define yield-handler 
  (make-parameter
   (lambda _
     (error "yield without bound handler"))))

(define (yield . args)
  (apply (yield-handler) args)
  (void))

(define-syntax-rule (with-consumer c e ...)
  (parameterize ((yield-handler c))
    e ...))

(define (producer-compose c . ps)
  (foldl
   (lambda (p c)
     (lambda args
       (with-consumer c (apply p args))))
   c ps))

(define (producer-chain . ps)
  (apply producer-compose (reverse ps)))

(module* main #f
  (for-each
   (lambda (x) (write `(applying ,x)) (newline) (x))
   (list 
    (producer-compose
     displayln
     (lambda (x) (yield x) (yield (+ x 1)))
     (thunk (for-each yield '(1 2 3))))
    (producer-compose
     displayln
     (lambda (x) (when (< x 3) (yield (list x x))))
     (thunk (for-each yield '(1 2 3)))))))
