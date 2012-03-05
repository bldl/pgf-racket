#lang racket

(require "prim.rkt")
(require "util.rkt")

;;; 
;;; Utility functions
;;; 

(define* (<+> x y)
  (concat x (text " ") y))

(define* (</> x y)
  (concat x (line) y))

(define* (folddoc f lst)
  ;; We could write (foldl f (nil) lst), but we want to avoid the
  ;; extra (nil) in the output, which could cause issues depending on
  ;; the passed 'f', say in adding separators between documents.
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

;; This function easily leads to a combinatorial explosion, and hence
;; we implement using lazy evaluation internally.
(define* (fill lst)
  (if (null? lst)
      (nil)
      (let ((x (car lst))
            (xs (cdr lst)))
        (if (null? xs)
            x
            (private-union/lazy
             (<+> (flatten x) (fill (cons (flatten (car xs)) (cdr xs))))
             (</> x (fill xs)))))))

;;; 
;;; Extra utility functions
;;; (Not in Wadler's paper.)
;;; 

(define (partition/2 lst)
  (let ((len (length lst)))
    (let-values (((q r) (quotient/remainder len 2)))
      (split-at lst (+ q r)))))

(define* group*
  (case-lambda
    (() (nil))
    ((x) (flatten x))
    (xs (let-values (((l r) (partition/2 xs)))
          (private-union (flatten (stack xs))
                         (stack (list (apply group* l)
                                      (apply group* r))))))))
