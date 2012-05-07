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
         )
    (list
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
