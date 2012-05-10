#lang racket

(require "prim.rkt")
(require "token.rkt")
(require "util.rkt")

;;; 
;;; shorthands
;;; 

;; backward compatibility
(define* to-token-stream identity)

;; backward compatibility
(define* cat tseq)

(define* (union l r (sh default-strength))
  (Union l r sh))

(define* (flatten x) ;; tseq -> tseq
  (private-flatten x))

(define* (group x (sh default-strength)) ;; tseq -> tseq
  (tseq (private-group x sh)))

(define* (group/cat . xs)
  (group xs))

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

;; xxx rewrite as lazy
(define* (sep-by sep elems)
  (let ((first #t)
        (r empty-tseq))
    (for ((x elems))
         (if first
             (set! first #f)
             (set! r (tseq-append r sep)))
         (set! r (tseq-append r x)))
    r))

(define* (nest n doc)
  (cat (indent n) doc dedent))

(define* (nest/str s doc)
  (cat (Nest (LvStr s)) doc dedent))

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

(define* (<+/> x y)
  (concat x (private-union (text " ") (line)) y))

|#

;;; 
;;; grouping input streams
;;; 

;; xxx problem -- have no way of adding to the stream -- need more control, better have our own stream type
;; (define* (group-stream* x)
;;   (define (f st s)
;;     (let loop ((st st) (s s))
;;       (if (tseq-empty? s)
;;           s ;; may get more later
;;           (let ((e (tseq-first s))
;;                 (t (tseq-rest s)))
;;             (cond
;;              ((Group? e)
;;               (
;;   (f '() (to-token-stream x)))

