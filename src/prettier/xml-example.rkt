#lang racket

#|

This example is also adapted from Wadler's paper, and it is an
important one as it makes use of 'fill', which is a troublesome
function, with the potential to build exponentially large documents in
the size of input.

The original in the paper seems somewhat buggy, so this adaptation is
also somewhat liberal. Not sure exactly what kind of output this is
supposed to produce either.

|#

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

(data Xml ((Elt s as xs) ;; string, list[Attr], list[Xml] -> Xml
           (Txt s))) ;; string -> Xml
(data Attr ((Att n v))) ;; string, string -> Attr

;; Xml -> DOC
(define* (showXML x)
  (folddoc concat (showXMLs x)))

;; Xml -> list[DOC]
(define (showXMLs x)
  (match
   x
   ((Elt n a c)
    (if (null? c)
        (list (concat (text "<") (showTag n a) (text "/>")))
        (list (concat (text "<") (showTag n a) (text ">")
                      (showFill showXML c)
                      (text "</") (text n) (text ">")))))
   ((Txt s)
    (list (fillwords s)))
   (_
    (error "showXMLs: unexpected" x))))

;; Attr -> DOC
(define (showAtt x)
  (let ((n (Att-n x))
        (v (Att-v x)))
    (concat (text n) (text "=") (text (quoted v)))))

;; string -> string
(define (quoted s)
  (string-append "\"" s "\""))

;; string, list[Attr] -> DOC
(define (showTag n a)
  (concat (text n) (showFill showAtt a)))

;; (any -> DOC), list[DOC] -> DOC
(define (showFill f xs)
  (if (null? xs)
      (nil)
      (bracket "" (fill (map f xs)) "")))

;; Xml
(define* xml-doc
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

