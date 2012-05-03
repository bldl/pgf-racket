#lang racket

(require "prim.rkt")
(require "spacer.rkt")
(require "token.rkt")
(require "util.rkt")

;;; 
;;; for testing the spacer engine
;;; 

(define (test-doc w fn f desc inToks)
  (printfln "// ~a (f=~a, w=~a)" desc fn w)
  (displayln (width-divider w))
  (let ((st (new-SpcSt f)))
    (let-values (((st outToks)
                  (process-tokens! st inToks)))
      (printlnTokenStream outToks)
      (pgf-println w outToks)))
  (displayln "// ----------"))

(define f-lst
  (list
   (cons "empty" decide-Nothing)
   (cons "always-space" always-Space)
   ))

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
         )
    (list
     (cons "nesting" d2)
     (cons "grouped then flattened" (flatten (group d1)))
     (cons "grouped" (group d1))
     (cons "flattened" (flatten d1))
     (cons "union" (stream (private-union (stream (Text "verylongverylong")) (stream (Text "short")))))
     (cons "lines" d1)
     (cons "word" (stream (Text "foobar")))
     (cons "empty" empty-stream)
     )))

(define (main)
  (for* ((w (reverse w-lst))
         (f f-lst)
         (d d-lst))
        (test-doc w (car f) (cdr f) (car d) (cdr d))))
      
(main)
