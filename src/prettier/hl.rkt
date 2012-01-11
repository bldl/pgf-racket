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

(define* (bracket l x r)
  (group (concat (text l)
                 (nest 2 (concat (line) x))
                 (line)
                 (text r))))

(define* (<+/> x y)
  (concat x (private-union (text " ") (line)) y))

(define* (fillwords s)
  (folddoc <+/> (map text (words s))))

(define* (cpp-directive s)
  (nest 2
        (folddoc
         (lambda (x y)
           (concat x (private-union (text " ") (line "\\")) y))
         (map text (words s)))))

(define* (cpp-if-else c-s t e)
  (concat (cpp-directive (string-append "#if " c-s)) (line)
          t (line)
          (text "#else") (line)
          e (line)
          (text "#endif")))

(define* (fill lst)
  (if (null? lst)
      (nil)
      (let ((x (car lst))
            (xs (cdr lst)))
        (if (null? xs)
            x
            (private-union
             (<+> (flatten x) (fill (flatten xs)))
             (</> x (fill xs)))))))
