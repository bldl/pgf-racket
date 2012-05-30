#lang racket

#|

The purpose of this module is to produce meaningful test data for
testing the token pipeline. What it does is translate a very small
Lisp of some kind to semantically annotated Lua source code tokens.

|#

(require "token.rkt")
(require "util.rkt")

(define* (lenv-new)
  '())

(define* (lenv-get env n)
  (let ((p (assq n env)))
    (and p (cdr p))))

(define* (lenv-put env n v)
  (cons (cons n v) env))

(define* (lenv-get/err env n)
  (aif v (lenv-get env n) v
       (error "undefined name" n)))

(struct NIL ())

(define* lnil (NIL))

(define (syntax-error e)
  (error "syntax error" e))

(define* (lapply env fe args)
  (let ((fv (leval env fe)))
    ;; xxx check fv is applicable
    ;; xxx check arity? lua semantics?
    (let ((avs (map (fix leval env) args)))
      (void)))) ;; xxx

;; This function evaluates expressions in the language directly,
;; without going through Lua.
(define* (leval env e)
  (cond
   ((eq? e 'nil) lnil)
   ((symbol? e) (lenv-get/err env e))
   ((number? e) e)
   ((string? e) e)
   ((boolean? e) e)
   ((list? e)
    (match e
           ((list-rest 'begin es)
            (if (null? es) lnil
                (for/last ((e es)) (leval env e))))
           ((list-rest f args)
            (lapply env f args))
           (else
            (syntax-error e))))
   (else (syntax-error e))))

;; This function compiles expressions in the language to Lua source
;; code tokens. The result can then be pretty printed and executed
;; using Lua, hopefully to the exactly same effect as with 'leval'.
;; Note that the generated token sequences have no whitespace to
;; separate tokens; that must be added prior to pretty printing.
(define* (lcompile env e)
  (void))

