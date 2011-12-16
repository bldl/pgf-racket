#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; Test code

(define (test w d)
  (pretty-nl (list w d (pretty w d))))

(for* ((w '(5 10 15 20 25))
       (d (list
           (text "foo")
           (text "foobar")
           (<+> (text "foo") (text "bar"))
           (</> (text "foo") (text "bar"))
           (spread (list (text "foo") (text "bar") (text "baz")))
           (stack (list (text "foo") (text "bar") (text "baz")))
           )))
      (test w d))

