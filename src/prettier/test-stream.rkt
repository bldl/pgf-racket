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

(define (mk-text n #:base (base "")) ;; integer -> tseq
  (map (compose Text (fix string-append base) number->string)
       (for/list ((i n)) i)))

(define (mk-text/lines n #:base (base "")) ;; integer -> tseq
  (add-between (mk-text n #:base base) br))

;; A grouping may produce output with groupings.
(define (foobar-f x)
  (tseq fill/ (mk-text/lines 6 #:base "filled") /fill))
(define foobar-grouping
  (make-grouping/tseq-call 'foobar foobar-f))
(define foobar/ (Begin foobar-grouping))
(define /foobar (End foobar-grouping))

(define (my-together . x)
  (UserToken
   (lambda (st tok)
     (struct-copy FmtSt st
                  (inDoc (tseq-append x (FmtSt-inDoc st)))))))

(struct Space UserToken (s) #:transparent)

(define (my-space s)
  (Space
   (lambda (st tok)
     (struct-copy FmtSt st
                  (inDoc (tseq-cons (union (Space-s tok) br) (FmtSt-inDoc st)))))
   s))

(define msp (my-space " "))

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
     ;;(cons "open/ close/ mismatch" (tseq group/ fillwords/ "fill" "these" "words" /group /fillwords))
     (cons "simple fillwords/" (tseq fillwords/ "fill" "these" "words" /fillwords))
     ;;(cons "unclosed stream group (XML style)" (tseq group/ (mk-text/lines 6 #:base "word")))
     (cons "foobar grouping" (tseq "begin" sp foobar/ "anything" br "here" sp "will" br "vanish" /foobar sp "end"))

     (cons "simple stream group (XML style)" (tseq group/ "OneWord" /group))
     (cons "stream group (XML style)" (tseq group/ (mk-text/lines 6 #:base "word") /group))
     (cons "stream group and fill (tseq/gr)" (tseq gr fl (mk-text/lines 10) end br fl (mk-text/lines 20) end end))
     (cons "nested stream fills (tseq/gr)" (tseq fl "a" br fl fl "b" br fl (mk-text/lines 20) end br "c" end br "d" end end))
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
     (cons "MyTogether" (tseq "111" msp (my-together "222" msp "333") msp "444"))
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
  ;; Check that nothing is evaluated before needed.
  (let ((s (tseq-cons/lazy (debug "foo") (tseq/lazy (debug "bar")))))
    (displayln 'have-s)
    (tseq-first s)
    (displayln 'have-first)
    (tseq-rest s)
    (displayln 'have-rest)
    (tseq-first (tseq-rest s))
    (displayln 'have-last))
  ;; Check that nothing is evaluated twice despite "strange" tseq
  ;; structure.
  (let ((s (group/cat (tseq-append/lazy (tseq-put/lazy (tseq/lazy (debug "double-foo") br (debug "double-bar") br (debug "double-ugh") br (debug "double-baz") br (debug "double-bamf") br) (debug "double-added")) (debug "double-appended")))))
    (test-doc 3 "no double evaluation of union branches" s)))

;; See what tseq-put based tseq construction results in, and see what
;; is left after each "get" from the stream.
(define (test-tseq-get)
  (define mkw (compose Text (fix string-append "word") number->string))
  (let more ((s empty-tseq) (i 10))
    (if (> i 0)
        (more (tseq-put/lazy s (mkw i)) (- i 1))
        (begin
          (writeln `(initial ,s))
          (let loop ((s s))
            (let-values (((h t) (tseq-get s)))
              (when h
                (writeln `(head ,h tail ,t))
                (loop t))))))))

(define (main)
  ;;(test-laziness)
  ;;(test-tseq-get)
  (for* ((w (reverse w-lst))
         (d d-lst))
        ;(writeln d)
        (test-doc w (car d) (cdr d))))
      
(main)
