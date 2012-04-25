#lang racket

(require "prim.rkt")
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
         (d1 (stream (Text "first") (Line "")
                     (Text "second") (Line "")
                     (Text "third") (Line "")
                     (Text "fourth")))
         (d2 (stream (Text "foobar") (Nest (LvInc 4)) (Line "")
                     (Text "baz") (Nest (LvPop)) (Line "")
                     (Text "bamf")))
         )
    (list
     (cons "nesting" d2)
     (cons "grouped" (group d1))
     (cons "flattened" (flatten d1))
     (cons "union" (stream (private-union (stream (Text "verylongverylong")) (stream (Text "short")))))
     (cons "lines" d1)
     (cons "word" (stream (Text "foobar")))
     (cons "empty" empty-stream))
    ))

(define (main)
  (for* ((w (reverse w-lst))
         (d d-lst))
        (test-doc w (car d) (cdr d))))
      
(main)
