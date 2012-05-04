#lang racket

(require "prim.rkt")
(require "token.rkt")
(require "util.rkt")

;;; 
;;; shorthands
;;; 

(define* (to-token-stream x)
  (cond
   ((stream? x) x)
   ((Token? x) (stream x))
   ((string? x) (stream (Text x)))
   (else
    (error "to-token-stream: unsupported" x))))

(define* (cat . xs)
  (apply stream-append
         (map to-token-stream xs)))

(define* (union l r (sh default-strength))
  (stream (Union (to-token-stream l)
                 (to-token-stream r) sh)))

(define* (flatten x) ;; stream-like -> stream
  (private-flatten (to-token-stream x)))

(define* (group x) ;; stream-like -> stream
  (stream (private-group (to-token-stream x))))

(define* br (Line))

(define* nbsp (Text " "))

(define* sp (union nbsp br))

(define* (bsp sh)
  (union nbsp br sh))

(define* indent0 (Nest (LvAbs 0)))

(define* align (Nest (LvRel 0)))

(define* dedent (Nest (LvPop)))

(define* (indent n) (Nest (LvInc n)))

(define* (exdent n) (Nest (LvInc (- n))))



#|
(define* (nest n doc)
  (NEST (LvInc n) doc))

(define* (nest/str s doc)
  (NEST (LvStr s) doc))

(define* (nest/abs n doc)
  (NEST (LvAbs n) doc))

(define* nest/0 (fix nest/abs 0))

(define* (nest/rel n doc)
  (NEST (LvRel n) doc))
|#

;;; 
;;; ala Wadler
;;; 

#|
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
|#

;;; 
;;; experimental
;;; 

#|
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
|#
