#lang racket

#|

Adapted and modified from Anya's original version written in Rascal.
This variant uses decision functions instead of decision tables. And
instead of using Rascal's ADT annotations in tokens, we annotate
ranges, HTML style.

|#

(require "token.rkt")
(require "util.rkt")

(struct* SpcSt
         (
          ctx ;; current context (SpcCtx)
          tr ;; range semantics, "bag"/multiset (list of symbol)
          pt ;; last seen token (Token or #f)
          pr ;; semantics for last seen token (list of symbol)
          )
         #:transparent #:mutable)
         
(struct* SpcCtx
         (
          backCtx ;; previous context (SpcCtx or #f)
          f ;; decision function, yields Decision (function)
          )
         #:transparent)

;; #f or list of Decision are also valid decisions.
(data* Decision ((Insert tok) ;; Token -> Decision
                 (EnterSt f) ;; function -> Decision
                 (ExitSt) ;; -> Decision
                 (Skip) ;; -> Decision
                 ))

(define-syntax-rule*
  (decider (pt pr tt tr yield) body ...)
  (lambda (pt pr tt tr)
    (let* ((lst '())
           (yield (lambda decs
                    (set! lst (append lst decs)))))
      body ...
      ;;(writeln (list 'decisions lst))
      lst)))

(define sp (Union (Text " ") (Line)))

;; pt:: previous token (Token)
;; pr:: previous range (list of symbol)
;; tt:: this token (Token)
;; tr:: this range (list of symbol)
(define* (decide-nothing pt pr tt tr)
  #f)

(define* (always-Space pt pr tt tr)
  (if pt (Insert sp) #f))

;; function? -> SpcSt
(define* (new-SpcSt (f decide-nothing))
  (SpcSt (SpcCtx #f f) '() #f '()))

;; SpcSt, function -> SpcSt
(define (enter-SpcCtx st f)
  (let ((ctx (SpcSt-ctx st)))
    (struct-copy SpcSt st (ctx (SpcCtx ctx f)))))

;; SpcSt -> SpcSt
(define (exit-SpcCtx st)
  (let ((ctx (SpcCtx-backCtx st)))
    (unless ctx
      (error "exit-SpcCtx: exiting from last SpcCtx"))
    (struct-copy SpcSt st (ctx ctx))))

;; Like remq, but errors out if the value 'v' does not exist.
(define (remq/error v lst)
  (if (null? lst)
      (error "close annotation without open" v lst)
      (let ((h (car lst))
            (t (cdr lst)))
        (if (eq? v h)
            t
            (cons h (remq/error v t))))))

;; Like remq*, but errors out if any of the values in 'v-lst' do not
;; exist.
(define (remq/error* v-lst lst)
  (if (null? v-lst)
      lst
      (let ((h (car v-lst))
            (t (cdr v-lst)))
        (remq/error* t (remq/error h lst)))))

;; SpcSt, list -> SpcSt
(define (add-annos st lst)
  (let ((old (SpcSt-tr st)))
    (struct-copy SpcSt st (tr (append lst old)))))

;; SpcSt, list -> SpcSt
(define (del-annos st lst)
  (let ((old (SpcSt-tr st)))
    (struct-copy SpcSt st (tr (remq/error* lst old)))))

;; SpcSt, Token -> SpcSt
(define (next-token st tt)
  (struct-copy SpcSt st (pt tt) (pr (SpcSt-tr st))))

(define (apply-dec st tt outToks dec)
  (let ((skip? #f))
    (let next ((decs (list dec)))
      (if (null? decs)
          (values st (if (or skip? (Nil? tt)) outToks (tseq-put outToks tt)))
          (let ((dec (car decs))
                (decs (cdr decs)))
            (cond
             ((or (not dec) (null? dec))
              (void))
             ((list? dec)
              (set! decs (append dec decs)))
             ((Skip? dec)
              (set! skip? #t))
             ((Insert? dec)
              (set! outToks (tseq-put outToks (Insert-tok dec))))
             ((EnterSt? dec)
              (set! st (enter-SpcCtx st (EnterSt-f dec))))
             ((ExitSt? dec)
              (set! st (exit-SpcCtx st)))
             (else
              (error "space-token: unsupported decision" dec)))
            (next decs))))))

;; SpcSt, tseq, Token, tseq -> SpcSt, tseq, tseq
(define* (space-token st inToks tt outToks)
  (cond
   ((Anno? tt)
    (let ((lst (Anno-lst tt)))
      (values st
              (tseq-append (Anno/ lst) (Anno-m tt) (/Anno lst) inToks)
              outToks)))
   ((Anno/? tt)
    (values (add-annos st (Anno/-lst tt)) inToks outToks))
   ((/Anno? tt)
    (values (del-annos st (/Anno-lst tt)) inToks outToks))
   (else
    (let* ((pt (SpcSt-pt st))
           (pr (SpcSt-pr st))
           (tr (SpcSt-tr st))
           (ctx (SpcSt-ctx st))
           (f (SpcCtx-f ctx))
           (dec (f pt pr tt tr))
           (st (next-token st tt)))
      (let-values (((st outToks)
                    (apply-dec st tt outToks dec)))
        (values st inToks outToks))))))

;; SpcSt, tseq, tseq -> SpcSt, tseq
(define* (space-tokens st inToks (outToks empty-tseq))
  (let-values (((h t) (tseq-get inToks)))
    (if (not h)
        (values st outToks)
        (let-values (((st inToks outToks)
                      (space-token st t h outToks)))
          ;;(writeln (list 'inToks inToks))
          (space-tokens st inToks outToks)))))

(define (annos->string a)
  (apply string-append (add-between (map symbol->string a) " ")))

(define* (print-spaced toks (annos? #f) (st (new-SpcSt)))
  (let* ((add (lambda (a) (set! st (add-annos st a))))
         (del (lambda (a) (set! st (del-annos st a))))
         (get (thunk (SpcSt-tr st))))
    (for ((t (in-tseq toks)))
        (match t
          ((Union (Text " ") (Line)) (display "_"))
          ((Text " ") (display "~"))
          ((Text s) (display (format "[~a~a]" s (if annos? (let ((a (get))) (if (null? a) "" (format ":~a" (annos->string a)))) ""))))
          ((Line) (display " "))
          ((Anno/ a) (add a))
          ((/Anno a) (del a))
          ((Anno a (Nil)) (when annos? (display (format "(~a)" (annos->string a)))))
          ((Anno a m) (begin
                        (add a) 
                        (print-spaced m annos? st)
                        (del a)))
          ((Nil) (void))
          (else (display (format " ~s " t)))))))

(define* (print-spacedln toks (annos? #f))
  (print-spaced toks annos?) (newline))
