#lang racket

(require "token.rkt")
(require "util.rkt")

(define-syntax-rule
  (debug op arg ... last)
  (begin
    (writeln (list (quote op) arg ... last))
    (op arg ... last)))

(struct* FmtSt (
                cw ;; specified page width (integer, constant)
                w ;; page width (rational)
                outDoc ;; formatted document (tseq of Token)
                midDoc ;; read but unformatted tokens (tseq of Token)
                inDoc ;; unread input (tseq of Token)
                k ;; current column (integer)
                lvStack ;; nesting stack (stack of string)
                bt ;; backtracking state (if any; can be chained)
                grp ;; grouping state (GrpSt or #f)
                ) #:transparent)

;;; 
;;; grouping plugin mechanism
;;; 

#|

Machinery for turning group/End ranges into nested grouping
constructions.

The way we do this is that we have both an input stream and grouping
state in the algorithm. If there is any grouping state then the input
is fed into the grouping machinery. Some input may be held in the
grouping state before it becomes available to the algorithm proper.
Calling 'flush' will cause an error if there's an incomplete grouping.

|#

;; type:: grouping type (Grouping)
;; st:: grouping state (any)
;; outer:: outer grouping state (GrpSt or #f)
(struct GrpSt (type st outer) #:transparent)

;; Emits the specified tokens from a source group to an outer group.
;; The outer group may further emit tokens forward, all the way up to
;; the top-level. (As this function may cause actual token processing
;; and layout decisions, make sure that 'st' is consistent and
;; complete when calling this function. Any patching afterwards may
;; not help as copies may already have been created, say for
;; backtracking.)
;;
;; st:: formatting state (FmtSt)
;; sg:: source group (GrpSt)
;; s:: token stream to receive (tseq or #f)
;; Returns:: formatting state (FmtSt)
(define (grp-emit st sg s)
  (define (to-outer name tg)
    (let* ((g-type (GrpSt-type tg))
           (g-st (GrpSt-st tg))
           (accept (Grouping-accept g-type)))
      (let-values (((n-g-st r) (accept g-st s name)))
        (let* ((n-grp (struct-copy GrpSt tg (st n-g-st)))
               (n-st (struct-copy FmtSt st (grp n-grp))))
          (grp-emit n-st tg r)))))

  (define (to-top)
    (process-tseq/nogroupings st s))

  (if (not s)
      st
      (let* ((tg (GrpSt-outer sg)))
        (if tg
            (let* ((g-type (GrpSt-type sg))
                   (name (Grouping-name g-type)))
              (to-outer name tg))
            (to-top)))))

;; st:: formatting state (FmtSt)
;; Returns:: formatting state (FmtSt)
(define (grp-flush st)
  (let ((grp (FmtSt-grp st)))
    (when grp
      (let* ((g-type (GrpSt-type grp))
             (g-st (GrpSt-st grp))
             (name (Grouping-name g-type))
             (eof-f (Grouping-eof g-type)))
        (eof-f g-st name))))
  st)

;; st:: formatting state (FmtSt)
;; h:: Begin token (Token)
;; Returns:: formatting state (FmtSt)
(define (grp-begin st h)
  (let* ((outer (FmtSt-grp st))
         (g-type (Begin-grouping h))
         (g-st ((Grouping-new g-type)))
         (inner (GrpSt g-type g-st outer)))
    (struct-copy FmtSt st (grp inner))))

;; st:: formatting state (FmtSt)
;; Returns:: formatting state (FmtSt)
(define (grp-end st)
  (let ((grp (FmtSt-grp st)))
    (unless grp
      (error "unopened grouping: before"
             (tseq-take 5 (FmtSt-inDoc st))))
    (let* ((g-type (GrpSt-type grp))
           (g-st (GrpSt-st grp))
           (outer (GrpSt-outer grp))
           (end-f (Grouping-end g-type))
           (r (end-f g-st)))
      ;; Have outer context accept the synthesized tseq that is the
      ;; result of the grouping. Also pop the ending grouping.
      (grp-emit (struct-copy FmtSt st (grp outer)) grp r))))

;; st:: formatting state (FmtSt)
;; h:: token belonging in the grouping (Token)
;; Returns:: formatting state (FmtSt)
(define (grp-put st h)
  (let ((grp (FmtSt-grp st)))
    (let* ((g-type (GrpSt-type grp))
           (g-st (GrpSt-st grp))
           (put (Grouping-put g-type)))
      (let-values (((n-g-st r) (put g-st h)))
        (let* ((n-grp (struct-copy GrpSt grp (st n-g-st)))
               (n-st (struct-copy FmtSt st (grp n-grp))))
          (grp-emit n-st grp r))))))

;;; 
;;; stack
;;; 

(define (spush st t)
  (cons t st))

;;; 
;;; indentation
;;; 

(define (spaces n)
  (make-string n #\space))

(define (string-chop-n n s)
  (let* ((len (string-length s))
         (nlen (+ len n)))
    (if (> nlen 0)
        (substring s 0 nlen)
        "")))

;; st:: old indentation state (stack of string)
;; k:: current column (integer)
;; lv:: level specification (Lv)
;; Returns:: new indentation state (stack of string)
(define (margin st k lv)
  (cond
   ((LvInc? lv)
    (spush st
           (let ((s (car st))
                 (n (LvInc-n lv)))
             (if (>= n 0)
                 (string-append s (spaces n))
                 (string-chop-n n s)))))
   ((LvStr? lv)
    (spush st
           (string-append (car st) (LvStr-s lv))))
   ((LvAbs? lv)
    (spush st
           (let ((n (LvAbs-n lv)))
             (if (> n 0) (spaces n) ""))))
   ((LvRel? lv)
    (margin st k (LvAbs (+ k (LvRel-n lv)))))
   ((LvPop? lv)
    ;; Stack must remain non-empty.
    (if (or (null? st) (null? (cdr st)))
        (error "margin: LvPop without matching Lv push")
        (cdr st)))
   (else (error "margin: unexpected" lv))))

;;; 
;;; formatting algorithm
;;; 

;; w:: page width (integer)
;; inDoc:: unread input (tseq of Token, optional)
(define* (new-FmtSt w (inDoc empty-tseq))
  (FmtSt w w empty-tseq empty-tseq inDoc 0 '("") #f #f))

;; Flushes buffered documents, committing decisions made thus far.
;; After this it is safe to consume all of 'outDoc'. Note that this
;; just resets state, you'll want to ensure that 'inDoc' contents have
;; already been processed.
(define (flush st) ;; FmtSt -> FmtSt
  (struct-copy FmtSt (grp-flush st) (bt #f)))

;; Note that this function does not handle groupings.
;; st:: current state (FmtSt)
;; d:: token to process (Token)
;; Returns:: new state (FmtSt)
(define (process-token/mid st d)
  (let ((k (FmtSt-k st))
        (w (FmtSt-w st))
        (outDoc (FmtSt-outDoc st))
        (i (car (FmtSt-lvStack st))))
    (cond
     ((Nest? d)
      (struct-copy FmtSt st
                   (lvStack
                    (margin (FmtSt-lvStack st) k (Nest-lv d)))))
     ((Text? d)
      ;; Here we must check whether the text still fits. If it
      ;; doesn't, we'll only continue if we don't have a way back.
      (let ((s (Text-s d)))
        (let ((k (+ k (string-length s)))
              (bt (FmtSt-bt st)))
          (if (and bt (> k w))
              bt ;; backtrack
              (struct-copy FmtSt st
                           (k k) (outDoc (tseq-put outDoc d)))))))
     ((Line? d)
      ;; A break always fits, and then we're committed, and
      ;; won't backtrack from here.
      (struct-copy FmtSt st
                   (k (string-length i))
                   (bt #f)
                   (outDoc
                    (tseq-append outDoc
                                 (Text "\n")
                                 (Text i)))))
     ((Union? d)
      ;; Pick left option, leave right for backtracking.
      (let ((l (Union-l d))
            (r (Union-r d))
            (sh (Union-sh d))
            (midDoc (FmtSt-midDoc st)))
        (let ((r-st
               ;; xxx We have a problem here as well. Now any
               ;; groupings within unions would not get processed as
               ;; groupings, which is not right. Yet we cannot just
               ;; put them past any groupings either and hope to
               ;; preserve ordering.
               (struct-copy FmtSt st
                            (midDoc (tseq-cons r midDoc)))))
          (struct-copy FmtSt st
                       (w (sh (FmtSt-cw st) i k))
                       (inDoc
                        (tseq-append l (Width w) inDoc))
                       (bt r-st)))))
     ((Width? d)
      (struct-copy FmtSt st
                   (w (Width-w d))
                   (inDoc inDoc)))
     ((UserToken? d)
      ;; Our current design lends itself to creating a
      ;; simple but fairly powerful extension mechanism like
      ;; this. Just use a UserToken or a subtype to define
      ;; how the state should change.
      ((UserToken-f d) (struct-copy FmtSt st
                                    (inDoc inDoc)) d))
     ((Together? d)
      (struct-copy FmtSt st
                   (inDoc (tseq-append (Together-m d) inDoc))))
     (else (error "process-token/nogroupings: unexpected" d))
     )))

;; st:: current state (FmtSt)
;; Returns:: new state (FmtSt)
(define (process-token st)
  (let-values (((d midDoc) (tseq-get (FmtSt-midDoc st))))
    (if d
        (process-token/mid (struct-copy FmtSt st (midDoc midDoc)) d)
        (let-values (((d inDoc) (tseq-get (FmtSt-inDoc st))))
          (if (not d) st
              (let ((st (struct-copy FmtSt st (inDoc inDoc))))
                (cond
                 ((Begin? d)
                  (grp-begin st d))
                 ((End? d)
                  (grp-end st))
                 ((FmtSt-grp st)
                  (grp-put st d))
                 (else
                  (process-token/mid st d)))))))))

;; Whether the state has any data to be processed.
(define (FmtSt-pending? st) ;; FmtSt -> boolean
  (or (not (tseq-empty? (FmtSt-inDoc st)))
      (not (tseq-empty? (FmtSt-midDoc st)))))

;; Adds a tseq to input.
(define* (FmtSt-write st s) ;; FmtSt, tseq of Token -> St
  (struct-copy FmtSt st (inDoc (tseq-append (FmtSt-inDoc st) s))))

;; Adds a token to input.
(define* (FmtSt-put st t) ;; FmtSt, Token -> St
  (FmtSt-write st (tseq t)))

;; Processes tokens for as long as there is input.
(define (process-input st) ;; FmtSt -> FmtSt
  (let loop ((st st))
    (if (FmtSt-pending? st)
        (loop (process-token st))
        st)))

;;; 
;;; text output
;;; 

;; d:: formatted token
;; Returns:: pretty-printed string
(define* (pgf-string/token d) ;; Token -> string
  (cond
   ((Text? d)
    (Text-s d))
   (else
    (error "pgf-string/token: unexpected" d))))

;; ts:: formatted document
;; Returns:: pretty-printed string
(define* (pgf-string/tseq ts) ;; tseq of Token -> string
  (apply string-append
         (for/list ((t (in-tseq ts)))
                   (pgf-string/token t))))

(define* (pgf-string/st st)
  (pgf-string/tseq
   (FmtSt-outDoc
    (process-input st))))

;; w:: page width
;; ts:: formatted document
;; Returns:: pretty-printed string
(define* (pgf-string w ts) ;; integer, tseq -> string
  (pgf-string/st (new-FmtSt w ts)))

;; Clears output buffer by printing it all out.
(define* (pgf-print/st/buffered st (out (current-output-port)))
  (for ((t (in-tseq (FmtSt-outDoc st))))
       (display (pgf-string/token t) out))
  (struct-copy FmtSt st (outDoc empty-tseq)))

;; Processes as much input as is available, and prints as much as
;; safely can. Works incrementally so that printing happens as soon as
;; there is text ready for output.
(define* (pgf-print/st/safe st (out (current-output-port)))
  (let loop ()
    (unless (FmtSt-bt st)
      (set! st (pgf-print/st/buffered st out)))
    (set! st (process-token st))
    (if (FmtSt-pending? st) (loop) st)))

(define* (pgf-print/st/flush st (out (current-output-port)))
  (pgf-print/st/buffered (flush (pgf-print/st/safe st out)) out))

(define* (pgf-print w ts (out (current-output-port)))
  (pgf-print/st/flush (new-FmtSt w ts) out))

(define* (pgf-println w ts (out (current-output-port)))
  (pgf-print w ts out) (newline out))

;;; 
;;; grouping construct
;;; 

;; cw:: full page width (integer)
;; i:: current indentation string (string)
;; k:: current column (integer)
;; Returns:: left choice page width (number)
(define* (default-strength cw i k)
  cw)

;; l:: left choice (tseq of Token)
;; r:: right choice (tseq of Token)
;; sh:: page width computation function (function)
;; Returns:: tseq of Token
(define* (private-union l r (sh default-strength))
  (Union l r sh))

;; Behaves lazily.
(define* (private-flatten ts) ;; tseq of Token -> tseq of Token
  (if (tseq-empty? ts)
      ts
      (let ((t (tseq-first ts))
            (ts (tseq-rest ts)))
        (cond
         ((Line? t)
          (tseq-cons/lazy (Text " ") (private-flatten ts)))
         ((Union? t)
          (private-flatten (tseq-append/lazy (Union-l t) ts)))
         ((Together? t)
          (private-flatten (tseq-append/lazy (Together-m t) ts)))
         ((SpaceT? t)
          (private-flatten (tseq-cons/lazy (Text (SpaceT-s t)) ts)))
         (else
          (tseq-cons/lazy t (private-flatten ts)))))))

 ;; tseq of Token -> tseq of Token
(define* (private-group ts (sh default-strength))
  (private-union (private-flatten ts) ts sh))
