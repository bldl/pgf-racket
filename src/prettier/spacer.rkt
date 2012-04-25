#lang racket

#|

Adapted from Anya's original version (in Rascal).

|#

(require "util.rkt")

;; Context may also be #f.
;; SpacerMode is a client-chosen value.
(struct* SpacerContext
         (
          modeStack ;; stack of SpacerMode
          nextContext ;; SpacerContext
          nesting ;; integer
          table ;; hash[cons[TokenCat current, TokenCat lastSeen], Decision]
          lastSeen ;; TokenCat
          )
         #:transparent)

(data* Decision ((Insert tok) ;; Token -> Decision
                 (EnterContext newTable) ;; rel[TokenCat, TokenCat, Decision] -> Decision (a relation is a set of tuples)
                 (ExitContext) ;; -> Decision
                 (Nothing) ;; -> Decision
                 (Skip) ;; -> Decision
                 ))

(define* (new-SpacerContext (table (hash)))
  (SpacerContext '() #f 0 table ""))

(define* (enter-context ctx table)
  (struct-copy SpacerContext ctx
               (nextContext ctx)
               (nesting (+ (SpacerContext-nesting ctx) 1))
               (table table)))

(define* (exit-context ctx)
  (let ((next (SpacerContext-nextContext ctx)))
    (unless next
      (error "exit-context: exiting from last context"))
    (unless (= (SpacerContext-nesting next)
               (- (SpacerContext-nesting ctx) 1))
      (error "exit-context: nesting error"))
    (unless (equal? (SpacerContext-modeStack next)
                    (SpacerContext-modeStack ctx))
      (error "exit-context: mode stack nesting error"))
    next))

(define (stream-put s t)
  (stream-append s (stream t)))

;; stream, stream, SpacerContext -> stream, stream, SpacerContext
(define* (process-token inToks outToks ctx)
  (if (stream-empty? inToks)
      (values inToks outToks ctx)
      (let ((tok (stream-first inToks))
            (inToks (stream-rest inToks)))
        (let* ((cat (anno tok))
               (decision (hash-ref (SpacerContext-table ctx)
                                   (cons cat
                                         (SpacerContext-lastSeen ctx))
                                   #f)))
          (if (Skip? decision)
              (values inToks outToks ctx)
              (begin
                (cond
                 ((not decision)
                  (void))
                 ((Insert? decision)
                  (set! outToks (stream-put outToks (Insert-tok decision))))
                 ((EnterContext? decision)
                  (set! ctx (enter-context ctx (EnterContext-newTable decision))))
                 ((ExitContext? decision)
                  (set! ctx (exit-context ctx)))
                 (else
                  (error "process-token: unsupported decision" decision)))
                (values inToks
                        (stream-put outToks tok)
                        (struct-copy SpacerContext ctx
                                     (lastSeen cat)))))))))

;; stream, stream, SpacerContext -> stream, stream, SpacerContext
(define* (process-tokens inToks outToks ctx)
  (let next ()
    (unless (stream-empty? inToks)
      (set!-values (inToks outToks ctx) (process-token inToks outToks ctx))
      (next))
    (values inToks outToks ctx)))
