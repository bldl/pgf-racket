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

(define* (sep-by/elems sep elems)
  (add-between elems sep))

(define* (nest n doc)
  (cat (indent n) doc dedent))

(define* (nest/str s doc)
  (cat (Nest (LvStr s)) doc dedent))

;;; 
;;; ala Wadler
;;; 

(define* (stack s)
  (sep-by br s))

(define* (fill s)
  (sep-by sp s))

(define* (fillwords s)
  (fill (words s)))

(define* (stack/elems elems)
  (sep-by/elems br elems))

(define* (fill/elems elems)
  (sep-by/elems sp elems))


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

(struct* Begin Token () #:transparent)
(struct* End Token () #:transparent)
(struct* Group Begin () #:transparent)
(struct* Fill Begin () #:transparent)

;; This is the interface to implement for each type of grouping.
;; open?:: whether a token opens this grouping
;; new:: creates fresh state for this grouping
;; put:: buffers a token within region
;; accept:: accepts a token from an inner grouping into this one
;; end:: ends this grouping
;; eof:: handles an EOF within this grouping
(struct Grouping (name open? new put accept end eof) #:transparent)

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

(define g-lst
  (list
   (Grouping
    'group
    Group? ;; open?
    (lambda () (GSt empty-tseq)) ;; new
    g-put ;; put
    g-put ;; accept
    (lambda (st) ;; end
      (let ((ge (group (GSt-buf st))))
        ge))
    (lambda (st) (error "unclosed Group" (GSt-buf st))) ;; eof
    )
   (Grouping
    'fill
    Fill? ;; open?
    (lambda () (FSt empty-tseq '())) ;; new
    f-put ;; put
    f-put ;; accept
    (lambda (st) ;; end
      (let ((lst (cons (FSt-s st) (FSt-lst st))))
        (fill/elems (reverse lst))))
    (lambda (st) ;; eof
      (error "unclosed Fill"
             (cons (reverse (FSt-lst st)) (FSt-s st))))
    )
   ))

(define (get-grouping t)
  (or (findf (lambda (gr)
               (let ((open? (Grouping-open? gr)))
                 (open? t))) g-lst)
      (error "get-grouping: not found" t)))

;; Lazily turns group/End ranges into nested grouping constructions.
;;
;; Note that 's' must be complete, with matching numbers of opening
;; and closing tokens. We are currently not providing a way to suspend
;; and resume the streaming, and to put more data into the stream,
;; which would be useful for incremental operation. We could quite
;; easily refactor to allow for that, as we already internally
;; maintain explicit grouping state.
(define* (group-stream s)
  ;; type:: grouping type (Grouping)
  ;; st:: grouping state (any)
  (struct Grp (type st) #:transparent)

  ;; grp:: current grouping (Grp or #f)
  ;; outer:: outer state (St or #f)
  (struct St (grp outer) #:transparent)

  (let next ((st #f) (s s))
    (lazy ;; even laziness
     (let-values (((h t) (tseq-get s)))
       ;;(writeln (list 'h h 't t 'st st))
       (if (not h)
           (let ((outer (and st (St-outer st))))
             (when outer
               ((Grouping-eof (Grp-type outer)) (Grp-st outer)))
             s)
           (cond
            ((Begin? h)
             (let* ((g-type (get-grouping h))
                    (g-st ((Grouping-new g-type)))
                    (grp (Grp g-type g-st)))
               (next (St grp st) t)))
            ((End? h)
             (begin
               (unless st
                 (error "unopened grouping" h))
               (let* ((grp (St-grp st))
                      (g-type (Grp-type grp))
                      (end (Grouping-end g-type))
                      (g-st (Grp-st grp))
                      (g-tok (end g-st))
                      (outer (St-outer st)))
                 (if outer
                     (let ((name (Grouping-name g-type))
                           (st outer))
                       (let* ((outer (St-outer st))
                              (grp (St-grp st))
                              (g-type (Grp-type grp))
                              (g-st (Grp-st grp))
                              (accept (Grouping-accept g-type))
                              (n-g-st (accept g-st g-tok name))
                              (n-grp (Grp g-type n-g-st))
                              (n-st (St n-grp outer)))
                         (next n-st t)))
                     (tseq-cons g-tok (next #f t))))))
            (else
             (if st
                 (let* ((grp (St-grp st))
                        (g-type (Grp-type grp))
                        (put (Grouping-put g-type))
                        (g-st (put (Grp-st grp) h)))
                   (next (St (Grp g-type g-st) (St-outer st)) t))
                 (tseq-cons h (next st t))))
            ))))))

(define* gr (Group))
(define* fl (Fill))
(define* end (End))

(define* (tseq/gr . lst)
  (group-stream lst))
