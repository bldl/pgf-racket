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

(define w-lst '(5 10 15 25 35 55 75))

(define d-lst
  (let* (
         (d1 (stream (Text "first") (Line)
                     (Text "second") (Line)
                     (Text "third") (Line)
                     (Text "fourth")))
         (d2 (stream (Text "foobar") (Nest (LvInc 4)) (Line)
                     (Text "baz") (Nest (LvPop)) (Line)
                     (Text "bamf")))
         (pop (Nest (LvPop)))
         (d4 (cat
              (Text "'") (Text "(")
              align
              (group
               (sequence->stream 
                (add-between
                 (map (compose Text number->string) (for/list ((i 10)) i))
                 (Line)))) dedent
              (Text ")")))
         (d5 (group* (list (list (cat "foo +" "bar") (cat "+"))
                           (cat "1 -" "2 -" "3"))))
         (d6 (group* (list (list (cat "(foo +" "bar)") (cat "*"))
                           (cat "(1 -" "2 -" "3)"))))
         (d7 (group* (cat* (cat* "(1 +" "2) *") (cat* "(1 +" "2 +" (cat* "(3 +" "4 +" "5) +") "6)"))))
         (d8 (group* (cat* (cat* (cat* "(1 +" "2)") "*") (cat* "(1 +" "2 +" (cat* (cat* "(3 +" "4 +" "5)") "+") "6)"))))
         )
    (list
     (cons "normal grouping (basic)" (group (cat (group (cat "(1 +" br "2 +" br "3) *")) br (group (cat "(4 +" br "5 +" br "6) *")) br (group (cat "(7 +" br "8 +" br "9)")))))
     (cons "normal grouping (nested)" (cat (indent 2) (group/cat (group/cat "(11 +" br "22) *") br (group/cat "(1 +" br "2 +" br (group/cat "(3 +" br "4 +" br "5) +") br "6)")) dedent))
     (cons "comparison 1 (group)" (cat (indent 2) (group/cat (group/cat "(11 +" br "22) *") br (group/cat "(11 +" br "22 +" br (group/cat "(3 +" br "4 +" br "5) +") br "6)")) dedent))
     (cons "comparison 1 (group*)" (cat (indent 2) (group* (cat* (cat* "(11 +" "22) *") (cat* "(11 +" "22 +" (cat* "(3 +" "4 +" "5) +") "6)"))) dedent))
     (cons "deeper special grouping, indented" (cat (indent 2) (group* (cat* (cat* "(11 +" "22) *") (cat* "(1 +" "2 +" (cat* "(3 +" "4 +" "5) +") "6)"))) dedent))
     (cons "deeper special grouping, loose op" d8)
     (cons "deeper special grouping, tight op" d7)
     (cons "parenthesized special grouped" d6)
     (cons "special grouped" d5)
     (cons "grouped list" d4)
     (cons "nesting" d2)
     (cons "flattened" (flatten d1))
     (cons "grouped" (group d1))
     (cons "grouped then flattened" (flatten (group d1)))
     (cons "union" (stream (private-union (stream (Text "verylongverylong")) (stream (Text "short")))))
     (cons "lines" d1)
     (cons "word" (stream (Text "foobar")))
     (cons "empty" empty-stream)
     ;;(cons "too many pops" (stream (Nest (LvInc 2)) pop pop))
     )))

(define (main)
  (for* ((w (reverse w-lst))
         (d d-lst))
        ;;(writeln d)
        (test-doc w (car d) (cdr d)))

  ;; to test laziness
  #;
  (let ((s (stream-cons (displayln 'foo) (stream (displayln 'bar)))))
    (displayln 'have-s)
    (stream-first s)
    (displayln 'have-first)
    (stream-rest s)
    (displayln 'have-last))
  )
      
(main)
