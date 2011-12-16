#lang racket

(require "util.rkt")

(define-syntax-rule*
  (define/p (nm arg ...) body ...)
  (define (nm arg ...)
    (printfln "enter ~a ~s" (quote nm) (list arg ...))
    body ...))

(define-syntax-rule*
  (define/p* (nm arg ...) body ...)
  (define* (nm arg ...)
    (printfln "enter ~a ~s" (quote nm) (list arg ...))
    body ...))

