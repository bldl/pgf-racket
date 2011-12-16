#lang racket

(require "prim.rkt")
(require "util.rkt")

;;; Utility functions

(define* (<+> x y)
  (concat x (text " ") y))

(define* (</> x y)
  (concat x (line) y))

(define* (folddoc f lst)
  (if (null? lst)
      (nil)
      (let ((x (car lst))
            (xs (cdr lst)))
        (if (null? xs)
            x
            (f x (folddoc f xs))))))

(define* spread (fix folddoc <+>))

(define* stack (fix folddoc </>))
