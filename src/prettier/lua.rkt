#lang racket

#|

The purpose of this module is to produce meaningful test data for
testing the token pipeline. What it does is translate a very small
Lisp of some kind to semantically annotated Lua source code tokens.

TODO:
- prettify arithmetic, as per paper? -- can we do this with spacer?

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

(define (eval-binop op env es e)
  (let ((vs (map (fix leval env) es)))
    (for ((v vs))
        (unless (number? v)
          (type-error "expected number" v e)))
    (apply op vs)))

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
           ((list-rest '+ es) (eval-binop + env es e))
           ((list-rest '* es) (eval-binop * env es e))
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
           ((list-rest '* es)
            `(* ,@(map (fix lrename env) es)))
           ((list-rest es) ;; function application
            (map (fix lrename env) es))
           (else
            (syntax-error e))))
   (else e)))

;;; 
;;; compiler
;;; 

(define (lparenthesize e)
  (define (f ctx e)
    (cond
     ((pair? e)
      (match e
        ((list-rest '+ es)
         (let ((sub `(+ ,@(map (fix f '+) es)))
               (par (if (eq? ctx '*) 'parens 'no-parens)))
           `(,par ,sub)))
        ((list-rest '* es)
         `(no-parens (* ,@(map (fix f '*) es))))
        (else
         (map lparenthesize e))))
     (else e)))
  (f #f e))

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
(define (lreturnize e)
  (define (f tr? e)
    (cond
     ((list? e)
      (match e
             ((list-rest 'begin es)
              `(begin ,@(map/last lreturnize (fix f tr?) es)))
             ((list-rest 'let
                         (list (list ns es) ...)
                         bes)
              `(let ,(map list ns (map lreturnize es))
                 ,@(map/last lreturnize (fix f tr?) bes)))
             ((list-rest 'lambda (list-rest ans) bes)
              `(lambda ,ans ,@(map/last lreturnize (fix f #t) bes)))
             ((list-rest '+ es)
              (wreturn tr? `(+ ,@(map lreturnize es))))
             ((list-rest '* es)
              (wreturn tr? `(* ,@(map lreturnize es))))
             ((list-rest es)
              (wreturn tr? (map lreturnize es)))
             (else
              (syntax-error e))))
     ((null? e)
      (syntax-error e))
     (else
      (wreturn tr? e))))
  (f #f e))

;; Instantiates a semantic token.
(define (sem . annos)
  (Anno annos (Nil)))

(define-syntax-rule (define-sem n)
  (define n (sem (quote n))))

(define lparen (Anno '(lparen) "("))
(define rparen (Anno '(rparen) ")"))
(define comma (Anno '(comma) ","))
(define local-kw (Anno '(local-kw) "local"))
(define function-kw (Anno '(function-kw) "function"))
(define (binop x) (Anno '(binop) x))
(define block/ (Anno '(block/) (Nil)))
(define /block (Anno '(/block) (Nil)))
(define body/ (Anno '(body/) (Nil)))
(define /body (Anno '(/body) (Nil)))
(define stmt/ (Anno '(stmt/) (Nil)))
(define /stmt (Anno '(/stmt) (Nil)))

(define-sem return/)
(define-sem /return)

(define-sem sub/)
(define-sem /sub)

(define sub-lparen (Anno '(lparen sub/) "("))
(define sub-rparen (Anno '(rparen /sub) ")"))

(define indent/ (Nest (LvInc 2)))
(define /indent (Nest (LvPop)))

(define-syntax-rule (when-anno a range body ...)
  (when (memq (quote a) range) body ...))

(define decide-lua
  (decider
   (pt pr tt tr yield)
   (begin
     (cond
      ((memq 'comma pr) (yield (Insert sp)))
      ((memq 'binop pr)
       (yield (if (and (memq 'lparen tr) (memq 'sub/ tr))
                  (Insert br)
                  (Insert sp))))
      ((memq 'local-kw pr) (yield (Insert sp)))
      ((memq 'function-kw pr) (yield (Insert sp)))
      ((memq 'lparen pr) (yield (Insert align/)))
      )
     (when-anno sub/ tr (yield (Insert group/)))
     (when-anno /sub tr (yield (Insert /group)))
     (cond
      ((memq 'binop tr) (yield (Insert nbsp)))
      ((memq 'rparen tr) (yield (Insert /align)))
      ((memq 'block/ tr) (yield (Insert group/)))
      ((memq '/block tr) (yield (Insert /group)))
      ((memq 'body/ tr) (yield (Insert indent/) (Insert br)))
      ((memq '/body tr) (yield (Insert /indent) (Insert br)))
      ((memq '/stmt tr) (yield (Insert ";")))
      ((memq 'stmt/ tr) (yield (and (memq '/stmt pr) (Insert (Line)))))
      ((memq 'return/ tr) (yield (Insert indent/) (Insert "return") (Insert sp)))
      ((memq '/return tr) (yield (Insert /indent)))
      )
     )))

(define (map-stmt f lst)
  (map
   (lambda (x) (tseq stmt/ (f x) /stmt))
   lst))

;; This function compiles expressions in the language to Lua source
;; code tokens. The result can then be pretty printed and executed
;; using Lua, hopefully to the exactly same effect as with 'leval'.
;; Note that the generated token sequences have no whitespace to
;; separate tokens; that must be added prior to pretty printing.
(define* (ltokenize e)
  (cond
   ((eq? e 'nil) "nil")
   ((symbol? e) (symbol->string e))
   ((number? e) (number->string e))
   ((string? e) (format "~s" e))
   ((boolean? e) (if e "true" "false"))
   ((list? e)
    (match e
           ((list 'return e)
            (tseq return/ (ltokenize e) /return))
           ((list-rest 'begin es)
            ;; xxx optimize away nil case earlier
            ;; xxx empty statement is actually ";"
            (if (null? es) "nil"
                (tseq block/ "do" body/
                      (map-stmt ltokenize es)
                      /body "end" /block)))
           ((list-rest 'let
                       (list (list ns es) ...)
                       bes)
            ;; xxx optimize empty cases before here
            (tseq block/ "do" body/
                  (map
                   (lambda (n e)
                     (tseq stmt/ local-kw (ltokenize n)
                           (binop "=") (ltokenize e) /stmt))
                   ns es)
                  (map-stmt ltokenize bes)
                  /body "end" /block))
           ((list-rest 'lambda (list-rest ans) bes)
            (tseq block/ function-kw lparen
                  (add-between (map ltokenize ans) comma)
                  rparen body/ (map ltokenize bes) /body
                  "end" /block))
           ((list-rest '+ es)
            (add-between (map ltokenize es) (binop "+")))
           ((list-rest '* es)
            (add-between (map ltokenize es) (binop "*")))
           ((list 'parens e)
            (tseq sub-lparen (ltokenize e) sub-rparen))
           ((list 'no-parens e)
            (tseq sub/ (ltokenize e) /sub))
           ((list-rest f args) ;; function application
            (tseq
             (if (symbol? f)
                 (symbol->string f)
                 (tseq lparen (ltokenize f) rparen))
             lparen (add-between
                  (map ltokenize args)
                  comma) rparen))
           (else
            (syntax-error e))))
   (else (syntax-error e))))

(define lcompile (compose ltokenize lparenthesize lreturnize))

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
   '("long begin" (40) (begin 1 2 3 (let ((x 1)) x) (+ 1 2 3)))
   '("empty let" (80) (let () 1))
   '("arithmetic" (80) (* (+ 1 2) (+ 3 4)))
   '("big arithmetic" (80) (* (+ 1 2 (+ 3 4) 5) (+ (* 6 7) 8)))
   '("lambda" (80) (lambda () 555))
   '("lambda application" (80) ((lambda () 555)))
   '("simple let" (80) (let ((x 1)) x))
   '("nested let" (80) (let ((x 1)) (let ((x x)) x)))
   '("let over lambda" (80) (let ((f (lambda () 555))) (f)))
   '("let over lambda (identity)" (80) (let ((f (lambda (x) x))) (f 666)))
   '("lambda application (multiple args)" (80 40) ((lambda (x y z) (+ x y z)) 1 2 3))
   '("complex expression 1" (80 40) (let ((f (lambda (x) (* (+ 1 2 (+ 3 4) 5) (+ (* x 7) x)))) (a 5)) (let ((x a)) (begin 2 (+ (f x) 1)))))
   '("paper expression" (20 30 40) (* (+ 1 2) (+ 3 4 (+ 5 6 7) 8)))
   '("paper expression (modified 1)" (20 30 40) (* (+ 1 2) (* 3 4 (+ 5 6 7) 8)))
   '("paper expression (modified 2)" (20 30 40) (* (+ 1 2) (+ 3 4 (* 5 6 7) 8)))
   '("paper expression (function)" (20 30 40) (lambda () (* (+ 1 2) (+ 3 4 (* 5 6 7) 8))))
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
      (let ((le (lcompile e)))
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
