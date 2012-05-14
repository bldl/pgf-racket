#lang racket

(require "hl.rkt")
(require "introspect.rkt")
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
     (list "comparison 1 (group)" '(20) (group/cat (group/cat "(" align "1 +" br "2" dedent ") *") br (group/cat "(" align "3 +" br "4 +" br (group/cat "(" align "5 +" br "6 +" br "7" dedent ") +") br "8" dedent ")")))
     (list "comparison 1 (group with sp, less br)" '(20 40 30 15 10 7) (group/cat (group/cat "(" align "1 +" sp "2" dedent ") *") br (group/cat "(" align "3 +" sp "4 +" sp (group/cat "(" align "5 +" sp "6 +" sp "7" dedent ") +") sp "8" dedent ")")))
     (list "comparison 1 (group with sp, more br)" '(20 40 30 15 10 7) (group/cat (group/cat "(" align "1 +" sp "2" dedent ") *") br (group/cat "(" align "3 +" sp "4 +" br (group/cat "(" align "5 +" sp "6 +" sp "7" dedent ") +") br "8" dedent ")")))
     (list "comparison 1 (group/fill)" '(20 40 30 15 10 7) (group/fill (together (together "(" align "1 +" sp "2" dedent ") *") tsp (together "(" align "3 +" sp "4 +" tsp (together "(" align "5 +" sp "6 +" sp "7" dedent ") +") tsp "8" dedent ")"))))
     )))

(define (main)
  (for ((d d-lst))
       (let ((t (first d))
             (w-lst (second d))
             (d (third d)))
         (pretty-println (tseq-to-sexp (tseq-optimize d)))
         (for ((w w-lst))
              (test-doc w t d)))))
      
(main)
