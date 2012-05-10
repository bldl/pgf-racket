#lang racket

#|
|#

(require "util.rkt")

(data* Lv ((LvInc n) ;; integer -> Lv
           (LvStr s) ;; string -> Lv
           (LvAbs n) ;; integer -> Lv
           (LvRel n) ;; integer -> Lv
           (LvPop))) ;; -> Lv

;; FmtEngine supports Nest, Text, Line, Union, and Width.
;;
;; Spacer supports Space, Anno, and /Anno tokens, dropping them or
;; translating them into something else. Otherwise it is token
;; agnostic.
(data* Token ((Nest lv) ;; Lv -> Token
              (Text s) ;; string -> Token
              (Line) ;; -> Token
              (Union l r sh) ;; stream, stream, function -> Token
              (Width w) ;; rational -> Token
              (Space s sh) ;; string, rational -> Token
              (Anno lst) ;; list of symbol -> Token
              (/Anno lst) ;; list of symbol -> Token
              (Group) ;; -> Token
              (End) ;; -> Token
              ))

#|

Example usage:

(tseq-foreach writeln '("1" "2" "3"))

(let ((s (tseq-map 
          (lambda (t)
            (Text (string-append "x" (Text-s t))))
          '("1" "2" "3"))))
  ;;(pretty-println s)
  (tseq-foreach writeln s))

(for ((e (in-tseq '("1" "2" "3"))))
     (writeln e))

|#

(data* Tseq ())

(define* empty-tseq '())

(define* (tseq? x)
  (or (pair? x) (Token? x) (string? x) (Tseq? x) (promise? x)))

(define* (tseq-empty? s)
  (not (tseq-first s)))

(define* (tseq-first s)
  (let-values (((e r) (tseq-get s))) e))

(define* (tseq-rest s)
  (let-values (((e r) (tseq-get s)))
    (unless e (error "tseq-rest: empty stream" s))
    r))

(define* (tseq-cons e s)
  (cons e s))

(define* (tseq-put s e)
  (cons s e))

(define* (tseq-append . s-lst)
  s-lst)

;; Must account for all tseq constructors. But only here, thanks to
;; our curious design.
(define* (tseq-get s)
  (cond
   ((Token? s) (values s empty-tseq))
   ((string? s) (values (Text s) empty-tseq))
   ((null? s) (values #f s))
   ((pair? s) (let ((h (car s)))
                (if (not h)
                    (tseq-get (cdr s)) ;; allow #f within a list
                    (let-values (((hh ht) (tseq-get h)))
                      (if (not hh)
                          (tseq-get (cdr s))
                          (values hh (cons ht (cdr s))))))))
   ((promise? s) (tseq-get (force s)))
   (else (error "tseq-get: not a tseq" s))))

(define* (tseq-foreach f s)
  (let-values (((h t) (tseq-get s)))
    (when h
      (f h)
      (tseq-foreach f t))))

(define* (tseq-map f s)
  (let loop ((r empty-tseq) (s s))
    (let-values (((h t) (tseq-get s)))
      (if (not h) r
          (loop (tseq-put r (f h)) t)))))

(define* (in-tseq s)
  (make-do-sequence
   (thunk
    (values tseq-first ;; current position -> current element
            tseq-rest ;; current position -> next element
            s ;; initial position
            (negate tseq-empty?) ;; current position -> whether at end
            (negate false?) ;; current element -> whether at end
            #f))))
