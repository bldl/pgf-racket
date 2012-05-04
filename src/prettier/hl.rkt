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
   ((not x) empty-stream)
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

(define* (sep-by sep elems)
  (let ((sep (to-token-stream sep))
        (first #t)
        (r empty-stream))
    (for ((x elems))
         (if first
             (set! first #f)
             (set! r (stream-append r sep)))
         (set! r (stream-append r (to-token-stream x))))
    r))

(define* (nest n doc)
  (cat (indent n) doc dedent))

(define* (nest/str s doc)
  (cat (Nest (LvStr s)) doc dedent))

#|
(define* (nest/abs n doc)
  (NEST (LvAbs n) doc))

(define* (nest/rel n doc)
  (NEST (LvRel n) doc))
|#

;;; 
;;; ala Wadler
;;; 

(define* (stack doc)
  (sep-by (Line) doc))

(define* (fill elems)
  (sep-by sp elems))

(define* (fillwords s)
  (fill (words s)))

#|

(define* (<+> x y)
  (concat x (text " ") y))

(define* (</> x y)
  (concat x (line) y))

(define* spread (fix folddoc <+>))

(define* (bracket l x r)
  (group (concat (text l)
                 (nest 2 (concat (line) x))
                 (line)
                 (text r))))

(define* (<+/> x y)
  (concat x (private-union (text " ") (line)) y))

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
