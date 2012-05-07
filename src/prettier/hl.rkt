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
   ((sequence? x) (sequence->stream x))
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

(define* (group x (sh default-strength)) ;; stream-like -> stream
  (stream (private-group (to-token-stream x) sh)))

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

(define (seq-join sep x)
  (define (f s)
    (if (stream-empty? s)
        s
        (let ((e (stream-first s))
              (t (stream-rest s)))
          (if (stream-empty? t)
              (if (not (sequence? e))
                  (stream e)
                  (f (sequence->stream e)))
              (if (not (sequence? e))
                  (stream-cons e (stream-cons sep (f t)))
                  (stream-append
                   (f (sequence->stream e))
                   (stream-cons sep (f t))))))))
  (if (not (sequence? x))
      (stream x)
      (f (sequence->stream x))))

(define seq-flatten (fix seq-join nbsp))
(define seq-fill (fix seq-join (private-union (stream (Text " "))
                                              (stream (Line)))))
                               
;; (let ((lst (list (cat "1" "2" "3") (list (Text "4") (Text "5") (list (Text "6") (Text "7") (Text "8"))))))
;;   (for/list ((i (seq-flatten lst))) i))

;; nested sequences -> stream of Token
(define* (group* x)
  ;; Here we build a grouping such that either (1) the first element
  ;; is flattened and the next one follows on the same line (2) the
  ;; first element is flattened, with a line break following, (3)
  ;; group* is called to handle the first element, and a line break
  ;; follows. When we speak of flattening we here actually mean
  ;; joining with spaces. The input should not contain Line tokens.
  (define (g s break?)
    (if (stream-empty? s)
        s
        (let ((e (stream-first s))
              (t (stream-rest s)))
          (if (stream-empty? t)
              (if (not break?)
                  (seq-flatten e)
                  (union
                   (seq-flatten e)
                   (group* e)))
              (let ((flat
                     (stream-append (seq-flatten e)
                                    (union (stream-cons nbsp (g t #f))
                                           (stream-cons br (g t #t))))))
                (if (not break?)
                    flat
                    (union flat
                           (stream-append (seq-fill e)
                                          (stream-cons br (g t #t))))))))))
  (if (not (sequence? x))
      (stream x)
      (g (sequence->stream x) #t)))
  
