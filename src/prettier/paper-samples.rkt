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
         (when-narrow (lambda (w i k) (let ((si (string-length i))) (if (> si (/ w 2)) 0 w))))
         )
    (list
     (list "not same effect (subexpr broken)" '(20) (tseq "(" align/ (group/cat "3 +" sp "4 +" sp (group/cat "(5 +" sp "6 +" sp "7) +") sp "8" /align ")")))
     (let ((forever (union "while (true)" "for (;;)" when-narrow))
           (in (indent 2))
           (ex pop))
       (list "semantically equivalent" '(15) (tseq forever in sp "return;" ex)))
     (list "comparison 1 (group)" '(20) (group/cat (group/cat "(" align "1 +" br "2" dedent ") *") br (group/cat "(" align "3 +" br "4 +" br (group/cat "(" align "5 +" br "6 +" br "7" dedent ") +") br "8" dedent ")")))
     (list "comparison 1 (group with sp, less br)" '(20 40 30 15 10 7) (group/cat (group/cat "(" align "1 +" sp "2" dedent ") *") br (group/cat "(" align "3 +" sp "4 +" sp (group/cat "(" align "5 +" sp "6 +" sp "7" dedent ") +") sp "8" dedent ")")))
     (list "comparison 1 (group with sp, more br)" '(20 40 30 15 10 7) (group/cat (group/cat "(" align "1 +" sp "2" dedent ") *") br (group/cat "(" align "3 +" sp "4 +" br (group/cat "(" align "5 +" sp "6 +" sp "7" dedent ") +") br "8" dedent ")")))
     ;;(list "comparison 1 (group/fill)" '(20 40 30 15 10 7) (group/fill (together (together "(" align "1 +" sp "2" dedent ") *") tsp (together "(" align "3 +" sp "4 +" tsp (together "(" align "5 +" sp "6 +" sp "7" dedent ") +") tsp "8" dedent ")"))))
     (list "shorter, with same effect (thanks to preceding br)" '(20 40 30 15 10 7) (tseq "(" align/ (group/cat "3 +" sp "4 +" br (group/cat "(5 +" sp "6 +" sp "7) +") sp "8" /align ")")))
     (list "comparison 1 (group, with preceding br)" '(20 40 30 15 10 7) (tseq (group/cat (group/cat "(" align/ "1 +" sp "2" /align ") *") br (group/cat "(" align/ "3 +" sp "4 +" br (group/cat "(" align/ "5 +" sp "6 +" sp "7" /align ") +") sp "8" /align ")"))))
     (list "comparison 1 (group/ delimited, with preceding br)" '(20 40 30 15 10 7) (tseq group/ group/ "(" align/ "1 +" sp "2" /align ") *" /group br group/ "(" align/ "3 +" sp "4 +" br group/ "(" align/ "5 +" sp "6 +" sp "7" /align ") +" /group sp "8" /align ")" /group /group))
     (list "comparison 1 (tran)" '(20 40 30 15 10 7) (tran (together (together "(" align/ "1 +" sp "2" /align ") *") tsp (together "(" align/ "3 +" sp "4 +" tsp (together "(" align/ "5 +" sp "6 +" sp "7" /align ") +") tsp "8" /align ")"))))
     (list "comparison 1 (tran/ and /tran)" '(20 40 30 15 10 7) (tseq tran/ together/ together/ "(" align/ "1 +" sp "2" /align ") *" /together tsp together/ "(" align/ "3 +" sp "4 +" tsp together/ "(" align/ "5 +" sp "6 +" sp "7" /align ") +" /together tsp "8" /align ")" /together /together /tran))
     ;;(list "comparison 1 (not working tran)" '(20 40 30 15 10 7) (tran (tseq together/ together/ "(" align/ "1 +" sp "2" /align ") *" /together tsp together/ "(" align/ "3 +" sp "4 +" tsp together/ "(" align/ "5 +" sp "6 +" sp "7" /align ") +" /together tsp "8" /align ")" /together /together))) ;; this does not work now that groupings are not processed before layout
     )))

(define (main)
  (for ((d d-lst))
       (let ((t (first d))
             (w-lst (second d))
             (d (third d)))
         (for ((w w-lst))
              (test-doc w t d))
         (pretty-println (tseq-to-sexp (tseq-optimize d)))
         )))
      
(main)
