#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; C++ specific utilities

(define (sp)
  (private-union (text " ") (line)))

(define (br)
  (private-union (nil) (line)))

(define (ind doc)
  (nest 2 doc))

(define (cat . lst)
  (apply concat (filter identity lst)))

(define (parens doc)
  (concat (text "(") (br) doc (br) (text ")")))

(define (cpp-directive s)
  (nest 2
        (folddoc
         (lambda (x y)
           ;; This actually breaks the rule we have for UNION, but
           ;; line breaks are special.
           (concat x (private-union (text " ") (line " \\")) y))
         (map text (words s)))))

(define (cpp-if-else c-s t e)
  (concat (cpp-directive (string-append "#if " c-s)) (line)
          t (line)
          (text "#else") (line)
          e (line)
          (text "#endif")))

(define (c-block doc)
  (concat
   (nest 2 (concat
            (text "{") (line)
            doc)) (line)
   (text "}")))

(define (c-if c t (e #f))
  (cat
   (concat (ind (concat (text "if") (sp) (parens c))) (sp))
   (c-block t)))

;;; Test data

(define d-lst
  (let* (
         (true-1 (text "true"))
         (break-1 (text "break;"))
         (continue-1 (text "continue;"))
         (if-1 (c-if true-1 break-1))
         (cpp-1 (cpp-if-else "1" break-1 continue-1))
         )
    (list
     (cons "nothing" (nil))
     (cons "break statement" break-1)
     (cons "if statement" if-1)
     (cons "nested if statement with complex condition" (c-if (fillwords "1 && 2 && 1==2 && defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__") if-1))
     (cons "#if-else" (cpp-if-else "defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__" (text "return 1;") (text "return 2;")))
     (cons "nested CPP" (c-if true-1 cpp-1))
     )))

(define w-lst '(5 10 15 25 35 45 55))

;;; Test runner

(define (test-doc w t d)
  (printfln "~a (w=~a)" t w)
  (newline)
  (writeln (DOC-to-sexp d)) (newline)
  (displayln (pretty w d))
  (displayln "----------"))

(define (main)
  (for* ((w w-lst)
         (d d-lst))
        (test-doc w (car d) (cdr d))))
      
(main)
