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

(define* union private-union)

(define* flatten private-flatten)

(define* group private-group)

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

(define* (sep-by/tokens sep s)
  (tseq-add-between s sep))

(define* (sep-by/elems sep elems)
  (add-between elems sep))

(define* (nest n doc)
  (cat (indent n) doc dedent))

(define* (nest/str s doc)
  (cat (Nest (LvStr s)) doc dedent))

;;; 
;;; ala Wadler
;;; 

(define* (stack/tokens s)
  (sep-by/tokens br s))

(define* (fill/tokens s)
  (sep-by/tokens sp s))

(define* (stack/elems elems)
  (sep-by/elems br elems))

(define* (fill/elems elems)
  (sep-by/elems sp elems))

(define* (fillwords s)
  (fill/elems (words s)))

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

;; buf:: buffered tokens (tseq)
(struct GSt (buf) #:transparent)

;; s:: stream for current "word" (tseq or #f)
;; lst:: lst of "words" (list of tseq)
(struct FSt (s lst) #:transparent)

(define (g-put st e (name #f))
  (GSt (tseq-put (GSt-buf st) e)))

(define (f-put st e (name #f))
  (let ((s (FSt-s st))
        (lst (FSt-lst st)))
    (if (Line? e)
        (FSt empty-tseq (cons s lst))
        (FSt (tseq-put s e) lst))))

(define* group-grouping
   (Grouping
    'group
    (lambda () (GSt empty-tseq)) ;; new
    g-put ;; put
    g-put ;; accept
    (lambda (st) ;; end
      (let ((ge (group (GSt-buf st))))
        ge))
    (lambda (st) (error "unclosed Group" (GSt-buf st))) ;; eof
    ))

(define* fill-grouping
   (Grouping
    'fill
    (lambda () (FSt empty-tseq '())) ;; new
    f-put ;; put
    f-put ;; accept
    (lambda (st) ;; end
      (let ((lst (cons (FSt-s st) (FSt-lst st))))
        (fill/elems (reverse lst))))
    (lambda (st) ;; eof
      (error "unclosed Fill"
             (cons (reverse (FSt-lst st)) (FSt-s st))))
    ))

(define* gr (Begin group-grouping))
(define* fl (Begin fill-grouping))
(define* end (End))

;; for backward compatibility
(define* tseq/gr tseq)

;; for backward compatibility
(define* group-stream identity)

;;; 
;;; filling grouping
;;; 

;; The point of this construction is that the tseq abstraction will
;; not go inside it. Rather, it will return the whole thing as a
;; single token.
(define* (together . xs)
  (Together xs))

;; This kind of space appears as Line() in some choice contexts, and
;; as Text(s) in others. In effect it's less eager to break than a
;; Line().
(define* tsp (SpaceT " "))

;; This function makes it easier to build relatively complicated
;; Union-based constructions. It recognizes SpaceT and Together tokens
;; as domain-specific language and translates them away.
;;
;; It is possible to achieve something similar with 'group' and 'sp'
;; and 'br' instead of 'together' and 'sp' and 'tsp', respectively,
;; but the latter pair of constructions have an interdependency that
;; may be useful for certain layout effects. The behavior of 'tsp'
;; depends on how well neighboring 'together' constructions fit.
;;
;; For example, one may build a grouping such that either (1) the
;; first element is flattened and the next one follows on the same
;; line (2) the first element is flattened, with a line break
;; following, (3) group/fill is called to handle the first element,
;; and a line break follows.
(define* (group/fill x)
  ;; s:: input (tseq)
  ;; break?:: whether breaking first token is allowed (boolean)
  ;; br?:: whether first SpaceT token must be output as a Line (boolean)
  (define (g s break? #:tsp-br? (br? #f))
    (lazy
     (let-values (((e t) (tseq-get s)))
       (if (not e) s
           (let ((next? (not (tseq-empty? t))))
             (cond
              ((SpaceT? e)
               (cond
                (br? (tseq-cons br (g t #t)))
                ((not break?) (tseq-cons (flatten e) (g t break?)))
                ((not next?) (union (flatten e) br))
                (else (union (tseq-cons (flatten e) (g t #f))
                             (tseq-cons br (g t #t))))))
              ((Together? e)
               (let ((m (Together-m e)))
                 (cond
                  ((not break?) (tseq-append (flatten m) (g t #t)))
                  ((not next?) (union (flatten m) (group/fill m)))
                  (else (union (tseq-append (flatten m) (g t #f))
                               (tseq-append (union (flatten m)
                                                   (group/fill m))
                                            (g t #t #:tsp-br? #t)))))))
              (else
               (tseq-cons e (group/fill t)))))))))
  (g x #t))

