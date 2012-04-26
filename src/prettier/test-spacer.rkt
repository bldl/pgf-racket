#lang racket

(require "prim.rkt")
(require "spacer.rkt")
(require "token.rkt")
(require "util.rkt")

;;; 
;;; for testing the spacer engine
;;; 

(define (test-doc w tn t desc inToks)
  (printfln "// ~a (t=~a, w=~a)" desc tn w)
  (displayln (width-divider w))
  (let ((outToks empty-stream)
        (ctx (new-SpacerContext t)))
    (set!-values (inToks outToks ctx)
                 (process-tokens inToks outToks ctx))
    (printlnTokenStream outToks)
    (pgf-println w outToks))
  (displayln "// ----------"))

(define t-lst
  (list
   (cons "empty" (hash))
   ))

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
         (t t-lst)
         (d d-lst))
        (test-doc w (car t) (cdr t) (car d) (cdr d))))
      
(main)
