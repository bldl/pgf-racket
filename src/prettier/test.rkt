#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "tree-example.rkt")
(require "util.rkt")

;;; Test code

(define (test w d)
  (pretty-println (list w d (pretty w d))))

(define w-lst '(5 10 15 25 35 45 55))

#;
(for* ((w w-lst)
       (d (list
           (text "foo")
           (text "foobar")
           (<+> (text "foo") (text "bar"))
           (</> (text "foo") (text "bar"))
           (spread (list (text "foo") (text "bar") (text "baz")))
           (stack (list (text "foo") (text "bar") (text "baz")))
           )))
      (test w d))

(for* ((w w-lst)
       (test (list testtree testtree^)))
      (printfln "~a (w=~a)" test w)
      (newline)
      (test w)
      (newline))
