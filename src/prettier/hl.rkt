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

(define* (sep-by sep s)
  (tseq-add-between s sep))

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

;; xxx add support for 'fill' such that Line() separates the elements
;; of a fill list

;; Lazily turns Group/End ranges into nested 'group' constructions.
;;
;; Note that 's' must be complete, with matching numbers of opening
;; and closing tokens. We are currently not providing a way to suspend
;; and resume the streaming, and to put more data into the stream,
;; which would be useful for incremental operation. We could quite
;; easily refactor to allow for that, as we already internally
;; maintain explicit grouping state.
(define* (group-stream s)
  ;; ctor:: constructs tseq given buffer contents (function or #f)
  ;; buf:: buffered tokens (tseq)
  ;; outer:: outer grouping (St or #f)
  (struct St (ctor buf outer) #:transparent)

  (define (new-St)
    (St #f empty-tseq #f))
  
  (define (buf-put st e)
    (struct-copy St st (buf (tseq-put (St-buf st) e))))
  
  (let next ((st (new-St)) (s s))
    (lazy ;; even laziness
     (let-values (((h t) (tseq-get s)))
       ;;(writeln (list 'h h 't t 'st st))
       (let ((ctor (St-ctor st))
             (buf (St-buf st)))
         (if (not h)
             (begin
               (when ctor (error "unclosed grouping" buf))
               s)
             (cond
              ((Group? h)
               (let ((outer (and ctor st)))
                 (next (St group empty-tseq outer) t)))
              ((End? h)
               (begin
                 (unless ctor
                   (error "unopened grouping" h))
                 (let ((ge (ctor buf))
                       (outer (St-outer st)))
                   ;;(writeln (list 'outer outer))
                   (if outer
                       (next (buf-put outer ge) t)
                       (tseq-cons ge (next (new-St) t))))))
              (else
               (if ctor
                   (next (buf-put st h) t)
                   (tseq-cons h (next st t))))
              )))))))

(define* gr (Group))
(define* end (End))

(define* (tseq/gr . lst)
  (group-stream lst))

