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

(define (mk-text n) ;; integer -> tseq
  (map (compose Text number->string) (for/list ((i n)) i)))

(define (mk-text/lines n) ;; integer -> tseq
  (add-between (mk-text n) br))

(define d-lst
  (let* (
         (d1 (tseq (Text "first") (Line)
                   (Text "second") (Line)
                   (Text "third") (Line)
                   (Text "fourth")))
         (d2 (tseq (Text "foobar") (Nest (LvInc 4)) (Line)
                   (Text "baz") (Nest (LvPop)) (Line)
                   (Text "bamf")))
         (pop (Nest (LvPop)))
         (d4 (cat
              (Text "'") (Text "(")
              align
              (group
               (add-between (mk-text 10) (Line))) dedent
              (Text ")")))
         )
    (list
     (cons "stream group and fill (tseq/gr)" (tseq/gr gr fl (mk-text/lines 10) end br fl (mk-text/lines 20) end end))
     (cons "stream fill (tseq/gr)" (tseq/gr fl (mk-text/lines 20) end))
     (cons "stream group (tseq/gr)" (tseq/gr gr gr "(" align "1 +" br "2" dedent ") *" end br gr "(" align "3 +" br "4 +" br gr "(" align "5 +" br "6 +" br "7" dedent ") +" end br "8" dedent ")" end end))
     (cons "stream group (complex group)" (group-stream (tseq gr gr "(" align "1 +" br "2" dedent ") *" end br gr "(" align "3 +" br "4 +" br gr "(" align "5 +" br "6 +" br "7" dedent ") +" end br "8" dedent ")" end end)))
     (cons "stream group (double group)" (group-stream (tseq gr gr "1 +" br "2 +" br "3" end end)))
     (cons "stream group (actual group)" (group-stream (tseq gr "1 +" br "2 +" br "3" end)))
     (cons "stream group (seq)" (group-stream (tseq "1" align "2" dedent "3")))
     (cons "stream group (seq with breaks)" (group-stream (tseq "1" align br "2" dedent br "3")))
     (cons "stream group (single token)" (group-stream "group-stream"))
     (cons "normal grouping (basic)" (group (cat (group (cat "(1 +" br "2 +" br "3) *")) br (group (cat "(4 +" br "5 +" br "6) *")) br (group (cat "(7 +" br "8 +" br "9)")))))
     (cons "normal grouping (nested)" (cat (indent 2) (group/cat (group/cat "(11 +" br "22) *") br (group/cat "(1 +" br "2 +" br (group/cat "(3 +" br "4 +" br "5) +") br "6)")) dedent))
     (cons "comparison 1 (group)" (cat (indent 2) (group/cat (group/cat "(11 +" br "22) *") br (group/cat "(11 +" br "22 +" br (group/cat "(3 +" br "4 +" br "5) +") br "6)")) dedent))
     ;;(cons "comparison 1 (group*)" (cat (indent 2) (group* (cat* (cat* "(11 +" "22) *") (cat* "(11 +" "22 +" (cat* "(3 +" "4 +" "5) +") "6)"))) dedent))
     ;;(cons "deeper special grouping, indented" (cat (indent 2) (group* (cat* (cat* "(11 +" "22) *") (cat* "(1 +" "2 +" (cat* "(3 +" "4 +" "5) +") "6)"))) dedent))
     ;;(cons "deeper special grouping, loose op" (group* (cat* (cat* (cat* "(1 +" "2)") "*") (cat* "(1 +" "2 +" (cat* (cat* "(3 +" "4 +" "5)") "+") "6)"))))
     ;;(cons "deeper special grouping, tight op" (group* (cat* (cat* "(1 +" "2) *") (cat* "(1 +" "2 +" (cat* "(3 +" "4 +" "5) +") "6)"))))
     ;;(cons "parenthesized special grouped" (group* (list (list (cat "(foo +" "bar)") (cat "*")) (cat "(1 -" "2 -" "3)"))))
     ;;(cons "special grouped" (group* (list (list (cat "foo +" "bar") (cat "+")) (cat "1 -" "2 -" "3"))))
     (cons "grouped list" d4)
     (cons "nesting" d2)
     (cons "flattened" (flatten d1))
     (cons "grouped" (group d1))
     (cons "grouped then flattened" (flatten (group d1)))
     (cons "union" (tseq (private-union (tseq (Text "verylongverylong")) (tseq (Text "short")))))
     (cons "lines" d1)
     (cons "word" (tseq (Text "foobar")))
     (cons "empty" empty-tseq)
     ;;(cons "too many pops" (tseq (Nest (LvInc 2)) pop pop))
     )))

(define-syntax-rule
  (debug e)
  (let ((v e)) (writeln `(evaluated ,(quote e) as ,v)) v))

(define (test-laziness)
  (let ((s (tseq-cons/lazy (debug "foo") (tseq/lazy (debug "bar")))))
    (displayln 'have-s)
    (tseq-first s)
    (displayln 'have-first)
    (tseq-rest s)
    (displayln 'have-rest)
    (tseq-first (tseq-rest s))
    (displayln 'have-last)))

(define (main)
  ;;(test-laziness)
  (for* ((w (reverse w-lst))
         (d d-lst))
        ;(writeln d)
        (test-doc w (car d) (cdr d))))
      
(main)
