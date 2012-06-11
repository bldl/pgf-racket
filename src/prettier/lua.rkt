#lang racket

#|

The purpose of this module is to produce meaningful test data for
testing the token pipeline. What it does is translate a very small
Lisp of some kind to semantically annotated Lua source code tokens.

|#

(require "hl.rkt")
(require "prim.rkt")
(require "token.rkt")
(require "spacer.rkt")
(require "util.rkt")

(define syntax-error
  (case-lambda
    ((desc e)
     (error "syntax error" desc e))
    ((e)
     (error "syntax error" e))))

(define (ck-ident x)
  (unless (symbol? x)
    (syntax-error "not an identifier" x))
  x)

(define (ck-idents lst)
  (for ((x lst))
       (unless (symbol? x)
         (syntax-error "not an identifier" x)))
  lst)

(define type-error
  (case-lambda
    ((desc e ctx)
     (error (format "type error: ~s in ~s: ~a" e ctx desc)))))

;;; 
;;; environment
;;; 

(define* (lenv-new)
  '())

(define* (lenv-get env n)
  (let ((p (assq n env)))
    (and p (cdr p))))

(define* (lenv-put env n v)
  (cons (cons n v) env))

(define* (lenv-put-all env lst)
  (append lst env))

(define* (lenv-get/err env n)
  (aif v (lenv-get env n) v
       (error "undefined name" n)))

;;; 
;;; nil type
;;; 

(struct NIL ())

(define* lnil (NIL))

(define* (lnil? x) (eq? lnil x))

;;; 
;;; function type
;;; 

;; ans:: argument names (list of symbol)
;; env:: lexical environment
;; b:: function body expression
(struct Func (ans env b) #:transparent)

(define* (lapply env fe args)
  (let ((fv (leval env fe)))
    (unless (Func? fv)
      (error "lapply: not applicable" fv))
    (let ((ans (Func-ans fv)))
      (unless (= (length args) (length ans))
        (error "lapply: wrong arity" ans args))
      (let* ((avs (map (fix leval env) args))
             (fenv (Func-env fv))
             (benv (lenv-put-all fenv (map cons ans avs))))
        (leval benv (Func-b fv))))))

;;; 
;;; evaluator
;;; 

;; This function evaluates expressions in the language directly,
;; without going through Lua.
(define* (leval env e)
  ;;(writeln e)
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
           ((list-rest 'let
                       (list (list ns es) ...)
                       bes)
            (let* ((vs (map (fix leval env) es))
                   (benv (lenv-put-all env (map cons (ck-idents ns) vs))))
              (leval benv `(begin ,@bes))))
           ((list-rest 'lambda (list-rest ans) bes)
            (Func (ck-idents ans) env `(begin ,@bes)))
           ((list-rest '+ es)
            (let ((vs (map (fix leval env) es)))
              (for ((v vs))
                   (unless (number? v)
                     (type-error "expected number" v e)))
              (apply + vs)))
           ((list-rest f args) ;; function application
            (lapply env f args))
           (else
            (syntax-error e))))
   (else (syntax-error e))))

;;; 
;;; symbol renaming
;;; 

;; For making all local variable (and function) names unique,
;; throughout the program. Note that we use lenv-* functions, but
;; instead of values we map to unique names.

(define (rn-sym env n)
  (ck-ident n)
  (let ((un (gensym n)))
    (values (lenv-put env n un) un)))

(define (rn-syms env ns)
  (ck-idents ns)
  (let ((uns (map gensym ns)))
    (values (lenv-put-all env (map cons ns uns)) uns)))
  
(define (lrename env e)
  (cond
   ((symbol? e) (lenv-get/err env e))
   ((list? e)
    (match e
           ((list-rest 'begin es)
            `(begin ,@(map (fix lrename env) es)))
           ((list-rest 'let
                       (list (list ns es) ...)
                       bes)
            (let ((es (map (fix lrename env) es)))
              (let-values (((env ns) (rn-syms env ns)))
                `(let ,(map list ns es) ,@(map (fix lrename env) bes)))))
           ((list-rest 'lambda (list-rest ans) bes)
            (let-values (((env ns) (rn-syms env ans)))
              `(lambda ,ns ,@(map (fix lrename env) bes))))
           ((list-rest '+ es)
            `(+ ,@(map (fix lrename env) es)))
           ((list-rest es) ;; function application
            (map (fix lrename env) es))
           (else
            (syntax-error e))))
   (else e)))

;;; 
;;; compiler
;;; 

(define (statement? e)
  (and (pair? e)
       (case (car e)
         ((begin let) #t)
         (else #f))))

(define (map/last f g lst)
  (if (null? lst)
      lst
      (let ((h (car lst))
            (t (cdr lst)))
        (if (null? t)
            (list (g h))
            (cons (f h) (map/last f g t))))))

(define (wreturn tr? e)
  (if tr? `(return ,e) e))

;; Inserts 'return' keywords in the appropriate places; namely the
;; last expression of each function body must be made a return
;; statement.
(define (ltranslate e)
  (define (f tr? e)
    (cond
     ((list? e)
      (match e
             ((list-rest 'begin es)
              `(begin ,@(map/last ltranslate (fix f tr?) es)))
             ((list-rest 'let
                         (list (list ns es) ...)
                         bes)
              `(let ,(map list ns (map ltranslate es))
                 ,@(map/last ltranslate (fix f tr?) bes)))
             ((list-rest 'lambda (list-rest ans) bes)
              `(lambda ,ans ,@(map/last ltranslate (fix f #t) bes)))
             ((list-rest '+ es)
              (wreturn tr? `(+ ,@(map ltranslate es))))
             ((list-rest es)
              (wreturn tr? (map ltranslate es)))
             (else
              (syntax-error e))))
     ((null? e)
      (syntax-error e))
     (else
      (wreturn tr? e))))
  (f #f e))

(define lparen (Anno '(lparen) "("))
(define rparen (Anno '(rparen) ")"))
(define comma (Anno '(comma) ","))
(define function (Anno '(function) "function"))
(define return (Anno '(return) "return"))
(define (binop x) (Anno '(binop) x))

(define (decide-lua pt pr tt tr)
  (cond
   ((memq 'comma pr) (Insert sp))
   ((memq 'binop pr) (Insert sp))
   ((memq 'binop tr) (Insert nbsp))
   ((memq 'function pr) (Insert sp))
   ((memq 'return pr) (Insert sp))
   ((memq 'lparen pr) (Insert align/))
   ((memq 'rparen tr) (Insert /align))
   (else (Nothing))))

;; This function compiles expressions in the language to Lua source
;; code tokens. The result can then be pretty printed and executed
;; using Lua, hopefully to the exactly same effect as with 'leval'.
;; Note that the generated token sequences have no whitespace to
;; separate tokens; that must be added prior to pretty printing.
(define* (lcompile e)
  (cond
   ((eq? e 'nil) "nil")
   ((symbol? e) (symbol->string e))
   ((number? e) (number->string e))
   ((string? e) (format "~s" e))
   ((boolean? e) (if e "true" "false"))
   ((list? e)
    (match e
           ((list 'return e)
            (tseq return (lcompile e)))
           ((list-rest 'begin es)
            (if (null? es) "nil"
                (tseq "do" (map lcompile es) "end")))
           ((list-rest 'let
                       (list (list ns es) ...)
                       bes)
            ;; We could enclose these in a block, but since the names
            ;; are unique there is little need for such clutter.
            (tseq (map
                   (lambda (n e)
                     (tseq "local" (lcompile n) (binop "=") (lcompile e)))
                   ns es)
                  (map lcompile bes)))
           ((list-rest 'lambda (list-rest ans) bes)
            (tseq function lparen
                  (add-between (map lcompile ans) comma)
                  rparen (map lcompile bes) "end"))
           ((list-rest '+ es)
            (add-between (map lcompile es) (binop "+")))
           ((list-rest f args) ;; function application
            (tseq
             (if (symbol? f)
                 (symbol->string f)
                 (tseq lparen (lcompile f) rparen))
             lparen (add-between
                  (map lcompile args)
                  comma) rparen))
           (else
            (syntax-error e))))
   (else (syntax-error e))))

;; we can run Lua as "lua -" and pipe the input via STDIN.

;;; 
;;; tests
;;; 

(define e-lst
  (list
   '("int literal" (80) 1)
   '("string literal" (80) "foo")
   '("boolean literal" (80) #t)
   '("empty begin" (80) (begin))
   '("short begin" (80) (begin 555))
   '("empty let" (80) (let () 1))
   '("lambda" (80) (lambda () 555))
   '("lambda application" (80) ((lambda () 555)))
   '("simple let" (80) (let ((x 1)) x))
   '("nested let" (80) (let ((x 1)) (let ((x x)) x)))
   '("let over lambda" (80) (let ((f (lambda () 555))) (f)))
   '("let over lambda (identity)" (80) (let ((f (lambda (x) x))) (f 666)))
   '("lambda application (multiple args)" (80) ((lambda (x y z) (+ x y z)) 1 2 3))
   ))

(define (lspace s)
  (let-values (((st outToks) 
                (space-tokens (new-SpcSt decide-lua) s)))
    outToks))

(define (test-exp w t e)
  (printfln "-- ~a (w=~a)" t w)
  (printfln "-- ~s [original]" e)
  (let ((e (lrename (lenv-new) e)))
    (printfln "-- ~s [renamed]" e)
    (let ((v (leval (lenv-new) e)))
      (printfln "-- --> ~s" v)
      (let ((le (lcompile (ltranslate e))))
        (print-spacedln le)
        (displayln (width-divider w))
        (pgf-println w (lspace le))
        (displayln "-------------")))))

(define (main)
  (for ((e-rec e-lst))
       (let ((t (first e-rec))
             (w-lst (second e-rec))
             (e (third e-rec)))
         (for ((w w-lst))
              (test-exp w t e))
         )))
      
(main)
