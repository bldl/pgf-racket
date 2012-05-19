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
                inDoc ;; unread input (tseq of Token)
                k ;; current column (integer)
                lvStack ;; nesting stack (stack of string)
                bt ;; backtracking state (if any; can be chained)
                grp ;; grouping state (GrpSt or #f)
                groups ;; grouping list (list of Grouping)
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

(struct* Begin Token () #:transparent)
(struct* End Token () #:transparent)
(struct* Group Begin () #:transparent)
(struct* Fill Begin () #:transparent)

;; This is the interface to implement for each type of grouping.
;; open?:: whether a token opens this grouping
;; new:: creates fresh state for this grouping
;; put:: buffers a token within region
;; accept:: accepts a token from an inner grouping into this one
;; end:: ends this grouping
;; eof:: handles an EOF within this grouping
(struct* Grouping (name open? new put accept end eof) #:transparent)

(define (get-grouping st t)
  (or (findf (lambda (gr)
               (let ((open? (Grouping-open? gr)))
                 (open? t)))
             (FmtSt-groups st))
      (error "get-grouping: not found" t)))

;; type:: grouping type (Grouping)
;; st:: grouping state (any)
;; outer:: outer grouping state (GrpSt or #f)
(struct GrpSt (type st outer) #:transparent)

;; st:: formatting state (FmtSt)
;; Returns:: formatting state (FmtSt)
(define (grp-flush st)
  (let ((grp (FmtSt-grp st)))
    (when grp
      ((Grouping-eof (GrpSt-type grp)) (GrpSt-st grp))))
  st)

;; st:: formatting state (FmtSt)
;; h:: Begin token (Token)
;; Returns:: formatting state (FmtSt)
(define (grp-begin st h)
  (let* ((outer (FmtSt-grp st))
         (g-type (get-grouping st h))
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
           (g-tok (end-f g-st)))
      (if outer
          ;; Have outer grouping accept the synthesized token that is
          ;; the result of the inner grouping.
          (let ((name (Grouping-name g-type))
                (grp outer))
            (let* ((g-type (GrpSt-type grp))
                   (g-st (GrpSt-st grp))
                   (accept (Grouping-accept g-type))
                   (n-g-st (accept g-st g-tok name))
                   (n-grp (struct-copy GrpSt grp (st n-g-st))))
              (struct-copy FmtSt st (grp n-grp))))
          ;; Unget synthesized token into the input stream.
          (struct-copy FmtSt st
                       (grp #f)
                       (inDoc (tseq-cons g-tok (FmtSt-inDoc st))))))))

;; st:: formatting state (FmtSt)
;; h:: token belonging in the grouping (Token)
;; Returns:: formatting state (FmtSt)
(define (grp-put st h)
  (let ((grp (FmtSt-grp st)))
    (let* ((g-type (GrpSt-type grp))
           (g-st (GrpSt-st grp))
           (put (Grouping-put g-type))
           (n-g-st (put g-st h))
           (n-grp (struct-copy GrpSt grp (st n-g-st))))
      (struct-copy FmtSt st (grp n-grp)))))

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
(define* (new-FmtSt w (inDoc empty-tseq)
                    #:groupings (groups '()))
  (FmtSt w w empty-tseq inDoc 0 '("") #f #f groups))

;; Flushes buffered documents, committing decisions made thus far.
;; After this it is safe to consume all of 'outDoc'. Note that this
;; just resets state, you'll want to ensure that 'inDoc' contents have
;; already been processed.
(define (flush st) ;; FmtSt -> FmtSt
  (struct-copy FmtSt (grp-flush st) (bt #f)))

(define (process-token st) ;; FmtSt -> FmtSt
  (let ((inDoc (FmtSt-inDoc st)))
    (let-values (((d inDoc) (tseq-get inDoc)))
      (if (not d)
          st
          (cond
           ((Begin? d)
            (grp-begin (struct-copy FmtSt st (inDoc inDoc)) d))
           ((End? d)
            (grp-end (struct-copy FmtSt st (inDoc inDoc))))
           ((FmtSt-grp st)
            (grp-put (struct-copy FmtSt st (inDoc inDoc)) d))
           (else
            (let ((k (FmtSt-k st))
                  (w (FmtSt-w st))
                  (outDoc (FmtSt-outDoc st))
                  (i (car (FmtSt-lvStack st))))
              (cond
               ((Nest? d)
                (struct-copy FmtSt st (inDoc inDoc)
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
                        (struct-copy FmtSt st (inDoc inDoc)
                                     (k k) (outDoc (tseq-put outDoc d)))))))
               ((Line? d)
                ;; A break always fits, and then we're committed, and
                ;; won't backtrack from here.
                (struct-copy FmtSt st (inDoc inDoc)
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
                      (sh (Union-sh d)))
                  (let ((r-st
                         (struct-copy FmtSt st
                                      (inDoc (tseq-append r inDoc)))))
                    (struct-copy FmtSt st
                                 (w (sh (FmtSt-cw st) i k))
                                 (inDoc
                                  (tseq-append l (Width w) inDoc))
                                 (bt r-st)))))
               ((Width? d)
                (struct-copy FmtSt st
                             (w (Width-w d))
                             (inDoc inDoc)))
               ((Together? d)
                (struct-copy FmtSt st
                             (inDoc (tseq-append (Together-m d) inDoc))))
               (else (error "process-token: unexpected" d))
               ))))))))

(define (FmtSt-eof? st) ;; FmtSt -> boolean
  (tseq-empty? (FmtSt-inDoc st)))

;; Adds a tseq to input.
(define* (FmtSt-write st s) ;; FmtSt, tseq of Token -> St
  (struct-copy FmtSt st (inDoc (tseq-append (FmtSt-inDoc st) s))))

;; Adds a token to input.
(define* (FmtSt-put st t) ;; FmtSt, Token -> St
  (FmtSt-write st (tseq t)))

;; Processes tokens for as long as there is input.
(define (process-tokens st) ;; FmtSt -> FmtSt
  (let loop ((st st))
    (if (FmtSt-eof? st)
        st
        (loop (process-token st)))))

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
    (process-tokens st))))

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
    (if (FmtSt-eof? st) st (loop))))

(define* (pgf-print/st/flush st (out (current-output-port)))
  (pgf-print/st/buffered (flush (pgf-print/st/safe st out)) out))

(define* (pgf-print w ts
                    (out (current-output-port))
                    #:groupings (groups '()))
  (pgf-print/st/flush (new-FmtSt w ts #:groupings groups) out))

(define* (pgf-println w ts
                      (out (current-output-port))
                      #:groupings (groups '()))
  (pgf-print w ts out #:groupings groups) (newline out))

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
