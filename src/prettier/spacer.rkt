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

;; pt:: previous token (Token)
;; pr:: previous range (list of symbol)
;; tt:: this token (Token)
;; tr:: this range (list of symbol)
(define* (decide-Nothing pt pr tt tr)
  (Nothing))

(define* (always-Space pt pr tt tr)
  (if pt (Insert (Space " " 1)) (Nothing)))

(define* (new-SpcSt (f decide-Nothing))
  (SpcSt (SpcCtx #f f) '() #f '()))

(define (enter-SpcCtx! st f)
  (let ((ctx (SpcSt-ctx st)))
    (set-SpcSt-ctx! st (SpcCtx ctx f)))
  st)

(define (exit-SpcCtx! st)
  (let ((ctx (SpcCtx-backCtx st)))
    (unless ctx
      (error "exit-SpcCtx!: exiting from last SpcCtx"))
    (set-SpcSt-ctx! st ctx)
    st))

(define (stream-put s t)
  ;; Better avoid set! here as 'stream' is a lazy construct.
  (let ((t 
         (if (Space? t)
             (Union (stream (Text (Space-s t)))
                    (stream (Line ""))
                    (Space-sh t)) t)))
    (stream-append s (stream t))))

(define (remq/error v lst)
  (if (null? lst)
      (error "close annotation without open" v lst)
      (let ((h (car lst))
            (t (cdr lst)))
        (if (eq? v h)
            t
            (cons h (remq/error v t))))))

(define (add-annos! st lst)
  (set-SpcSt-tr! st (append lst (SpcSt-tr st))))

(define (del-annos! st lst)
  (let ((annos (SpcSt-tr st)))
    (for ((a lst))
         (set! annos (remq/error a annos)))
    (set-SpcSt-tr! st annos)))

(define (next-token! st tt)
  (set-SpcSt-pt! st tt)
  (set-SpcSt-pr! st (SpcSt-tr st))
  st)

;; SpcSt, Token, stream -> SpcSt, stream
(define* (process-token! st tt outToks)
  (cond
   ((Anno? tt)
    (values (add-annos! st (Anno-lst tt)) outToks))
   ((/Anno? tt)
    (values (del-annos! st (Anno-lst tt)) outToks))
   (else
    (let* ((pt (SpcSt-pt st))
           (pr (SpcSt-pr st))
           (tr (SpcSt-tr st))
           (ctx (SpcSt-ctx st))
           (f (SpcCtx-f ctx))
           (dec (f pt pr tt tr)))
      (next-token! st tt)
      (if (Skip? dec)
          (values st outToks)
          (begin
            (cond
             ((Nothing? dec)
              (void))
             ((Insert? dec)
              (set! outToks (stream-put outToks (Insert-tok dec))))
             ((EnterSt? dec)
              (set! st (enter-SpcCtx! st (EnterSt-f dec))))
             ((ExitSt? dec)
              (set! st (exit-SpcCtx! st)))
             (else
              (error "process-token!: unsupported decision" dec)))
            (values st (stream-put outToks tt))))))))

;; SpcSt, sequence, stream -> SpcSt, stream
(define* (process-tokens! st inToks (outToks empty-stream))
  (for ((tt inToks))
       (set!-values (st outToks) (process-token! st tt outToks)))
  (values st outToks))

(define* (printlnTokenStream toks)
  (for ((t (in-stream toks)))
       (match t
              ((Text " ") (display "_"))
              ((Text s) (display (format "[~a]" s)))
              ((Line s) (display " "))
              (else (display (format " ~s " t)))))
  (newline))
