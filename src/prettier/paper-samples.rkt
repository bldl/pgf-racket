#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "token.rkt")
(require "util.rkt")

;;; 
;;; for testing the stream-based API
;;; 

(define (test-doc w t d)
  (printfln "// ~a (w=~a)" t w)
  (displayln (width-divider w))
  (pgf-println w d)
  (displayln "// ----------"))

(define d-lst
  (let* (
         (pop (Nest (LvPop)))
         )
    (list
     (list "comparison 1 (group)" '(20) (cat (indent 2) (group/cat (group/cat "(1 +" br "2) *") br (group/cat "(3 +" br "4 +" br (group/cat "(5 +" br "6 +" br "7) +") br "8)")) dedent))
     (list "comparison 1 (group*)" '(20) (cat (indent 2) (group* (cat* (cat* "(1 +" "2) *") (cat* "(3 +" "4 +" (cat* "(5 +" "6 +" "7) +") "8)"))) dedent))
     )))

(define (main)
  (for ((d d-lst))
       (let ((t (first d))
             (w-lst (second d))
             (d (third d)))
         (for ((w w-lst))
              (test-doc w t d)))))
      
(main)
