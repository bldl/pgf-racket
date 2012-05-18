#lang racket

#|

Here we attempt to create an example for the paper demonstrating our
strength factor and the "narrowing" of equivalent constructs. We adapt
Wadler's XML example for this purpose.

One thing to note here is that 'flatten' does away with any 'Union'
within it, and the left choice is taken without any regard to the
strength factor. (Naturally the same is true of 'group' and
'group/fill' as they use 'flatten' internally.) This is clearly a
problem in that it pretty much ruins the algebra. Unless we can
account for the strength factor when flattening a Union. But this does
seem unlikely.

|#

(require "hl.rkt")
(require "introspect.rkt")
(require "prim.rkt")
(require "token.rkt")
(require "util.rkt")

(define-syntax-rule
  (debug v)
  (begin
    (pretty-println `(v ,v))
    v))

(data Xml ((Elt s as xs) ;; string, list[Attr], list[Xml] -> Xml
           (Txt s))) ;; string -> Xml
(data Attr ((Att n v))) ;; string, string -> Attr

(define text Text)

;; string -> string
(define (quoted s)
  (format "~s" s))

(define IN (indent 2))
(define EX dedent)

(define (empty-elt->tseq/basic n a)
  (tseq (text "<") (tag->tseq n a) (text "/>")))

#|

(define (disallow-narrow w i k)
  (let ((si (string-length i)))
    (if (>= si (/ w 10)) 0 w)))

(define (empty-elt->tseq n a)
  (union
   (tseq (text "<") (tag->tseq n a) (text ">")
         (text "</") (text n) (text ">"))
   (empty-elt->tseq/basic n a)
   disallow-narrow))

|#

(define (empty-elt->tseq n a)
  (union
   (tseq (text "<") (tag->tseq n a) (text ">")
         (text "</") (text n) (text ">"))
   (tseq (text "<") (tag->tseq n a) (text "/>"))
   (lambda (w i k) (debug k)
     ;;(if (>= (string-length i) (/ w 10)) 0 w)
     (if (> k (/ w 2)) 0 w)
     )))

;; Xml -> DOC
(define (showXML x)
  (match
   x
   ((Elt n a c)
    (if (null? (debug c))
        (empty-elt->tseq n a)
        (tseq (text "<") (tag->tseq n a) (text ">")
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
      ;;(tseq IN br (sep-by/elems br (map f xs)) EX br)
      ;;(tseq IN br (group (sep-by/elems sp (map f xs))) EX br)
      (tseq IN br (group/fill (sep-by/elems tsp (map (compose together f) xs))) EX br)
      ))

;; Attr -> DOC
(define (showAtt x)
  (let ((n (Att-n x))
        (v (Att-v x)))
    (tseq sp (text n) (text "=") (text (quoted v)))))

;; string, list[Attr] -> DOC
(define (tag->tseq n a)
  (tseq (text n) IN (map showAtt a) EX))

;;; 
;;; test data generator
;;; 

(define-syntax-rule
  (times/list n expr)
  (for/list ((i (in-range n))) expr))

(define (one-in? n)
  (= (random n) 0))

(define ascii-lst
  (for/list ((i (in-range 128)))
            (integer->char i)))
(define lower-lst (filter char-lower-case? ascii-lst))
(define upper-lst (filter char-upper-case? ascii-lst))
(define alpha-lst (append lower-lst upper-lst))
(define underscore #\_)

(define (vowel? c)
  (memv c (list #\a #\e #\i #\o #\u #\y
                #\A #\E #\I #\O #\U #\Y)))
(define (underscore? c)
  (eqv? c underscore))
(define (vowel/us? c)
  (or (vowel? c) (underscore? c)))

(define (random/from-list lst)
  (list-ref lst (random (length lst))))

(define (random/from-range a b)
  (+ a (random (+ (- b a) 1))))

(define (random-string n lst)
  (apply string (times/list n (random/from-list lst))))

(define (random-string/readable n alphabet #:capitalize? (cap #f))
  (let next ((n n) (lst '()) (was? #t))
    (if (> n 0)
        (let* ((c (random/from-list alphabet))
               (v (vowel/us? c))
               (ok (or v was?)))
          (if ok
              (let ((c (if (and cap (null? lst)) (char-upcase c) c)))
                (next (- n 1) (cons c lst) v))
              (next n lst was?)))
        (apply string (reverse lst)))))

(define (random-sentence)
  (let ((s (apply string-append
                  (add-between
                   (times/list (+ 3 (random 30))
                               (random-string/readable
                                (random/from-range 3 10)
                                lower-lst))
                   " "))))
    (string-set! s 0 (char-upcase (string-ref s 0)))
    (string-append s ".")))

(define (random-name)
  (random-string/readable (random/from-range 1 1) lower-lst))

(define (random-att)
  (Att (random-name)
       (random-string/readable (random/from-range 2 2) lower-lst)))

(define (random-txt)
  (Txt (random-sentence)))

(define (deeper? lv)
  (> (- 10 lv) (random 12)))

(define (random-elt (lv 1))
  (Elt (random-name)
       (times/list (random/from-range 0 2) (random-att))
       (cond
        ((one-in? 3) '())
        ((deeper? lv) (times/list 3 (random-elt (+ lv 1))))
        ((one-in? 2) (list (random-txt)))
        (else '())
        )))

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
         )
    (list
     (list "random XML" '(40) (showXML (random-elt)))
     ;;(list "XML document" '(80 25) (showXML xml-doc-1))
     )))

(define (main)
  (for ((d d-lst))
       (let ((t (first d))
             (w-lst (second d))
             (d (third d)))
         (for ((w w-lst))
              (test-doc w t d))
         ;;(pretty-println (tseq-to-sexp (tseq-optimize d)))
         )))
      
(main)

