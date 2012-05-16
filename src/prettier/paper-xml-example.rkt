#lang racket

#|

Here we attempt to create an example for the paper demonstrating our
strength factor and the "narrowing" of equivalent constructs. We adapt
Wadler's XML example for this purpose.

|#

(require "hl.rkt")
(require "introspect.rkt")
(require "prim.rkt")
(require "token.rkt")
(require "util.rkt")

(data Xml ((Elt s as xs) ;; string, list[Attr], list[Xml] -> Xml
           (Txt s))) ;; string -> Xml
(data Attr ((Att n v))) ;; string, string -> Attr

(define text Text)

;; string -> string
(define (quoted s)
  (format "~s" s))

;; Xml -> DOC
(define (showXML x)
  (match
   x
   ((Elt n a c)
    (if (null? c)
        (tseq (text "<") (showTag n a) (text "/>"))
        (tseq (text "<") (showTag n a) (text ">")
              (showFill showXML c)
              (text "</") (text n) (text ">"))))
   ((Txt s)
    (fillwords s))
   (_
    (error "showXMLs: unexpected" x))))

;; (any -> DOC), list[DOC] -> DOC
(define (showFill f xs)
  (if (null? xs)
      empty-tseq
      (group/cat (indent 2) br (fill/elems (map f xs)) dedent br)))

;; Attr -> DOC
(define (showAtt x)
  (let ((n (Att-n x))
        (v (Att-v x)))
    (tseq (text n) (text "=") (text (quoted v)))))

;; string, list[Attr] -> DOC
(define (showTag n a)
  (tseq (text n) (showFill showAtt a)))

;;; 
;;; test runner
;;; 

(define (test-doc w t d)
  (printfln "// ~a (w=~a)" t w)
  (displayln (width-divider w))
  (pgf-println w d)
  (displayln "// ----------"))

(define xml-doc-1
  (Elt "p"
       (list (Att "color" "red") (Att "font" "Times") (Att "size" "10"))
       (list
        (Txt "Here is some")
        (Elt "em" '() (list (Txt "emphasized")))
        (Txt "text.")
        (Txt "Here is a")
        (Elt "a"
             (list (Att "href" "http://www.eg.com/"))
             (list (Txt "link")))
        (Txt "elsewhere."))))

(define d-lst
  (let* (
         (pop (Nest (LvPop)))
         (when-narrow (lambda (w i k) (let ((si (string-length i))) (if (> si (/ w 2)) 0 w))))
         )
    (list
     (list "XML document" '(80 25) (showXML xml-doc-1))
     )))

(define (main)
  (for ((d d-lst))
       (let ((t (first d))
             (w-lst (second d))
             (d (third d)))
         ;;(pretty-println (tseq-to-sexp (tseq-optimize d)))
         (for ((w w-lst))
              (test-doc w t d)))))
      
(main)

