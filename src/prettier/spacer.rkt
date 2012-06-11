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

(data* Decision ((Insert tok) ;; Token -> Decision
                 (EnterSt f) ;; function -> Decision
                 (ExitSt) ;; -> Decision
                 (Nothing) ;; -> Decision
                 (Skip) ;; -> Decision
                 ))

(define sp (Union (Text " ") (Line)))

;; pt:: previous token (Token)
;; pr:: previous range (list of symbol)
;; tt:: this token (Token)
;; tr:: this range (list of symbol)
(define* (decide-Nothing pt pr tt tr)
  (Nothing))

(define* (always-Space pt pr tt tr)
  (if pt (Insert sp) (Nothing)))

;; function? -> SpcSt
(define* (new-SpcSt (f decide-Nothing))
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

;; SpcSt, Token, tseq -> SpcSt, tseq
(define* (space-token st tt outToks)
  (cond
   ((Anno/? tt)
    (values (add-annos st (Anno/-lst tt)) outToks))
   ((/Anno? tt)
    (values (del-annos st (/Anno-lst tt)) outToks))
   (else
    (let* ((pt (SpcSt-pt st))
           (pr (SpcSt-pr st))
           (tr (SpcSt-tr st))
           (ctx (SpcSt-ctx st))
           (f (SpcCtx-f ctx))
           (dec (f pt pr tt tr))
           (st (next-token st tt)))
      (begin
        (cond
         ((Skip? dec)
          (values st outToks))
         ((Nothing? dec)
          (values st (tseq-put outToks tt)))
         ((Insert? dec)
          (values st (tseq-append outToks (Insert-tok dec) tt)))
         ((EnterSt? dec)
          (values (enter-SpcCtx st (EnterSt-f dec)) (tseq-put outToks tt)))
         ((ExitSt? dec)
          (values (exit-SpcCtx st) (tseq-put outToks tt)))
         (else
          (error "space-token: unsupported decision" dec))))))))

;; SpcSt, tseq, tseq -> SpcSt, tseq
(define* (space-tokens st inToks (outToks empty-tseq))
  (for ((tt (in-tseq inToks)))
       (set!-values (st outToks) (space-token st tt outToks)))
  (values st outToks))

(define* (printlnTokenStream toks)
  (for ((t (in-tseq toks)))
      (match t
        ((Union (Text " ") (Line)) (display "_"))
        ((Text " ") (display "~"))
        ((Text s) (display (format "[~a]" s)))
        ((Line) (display " "))
        (else (display (format " ~s " t)))))
  (newline))
