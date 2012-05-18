#lang racket

(require "token.rkt")
(require "util.rkt")

(define-syntax-rule
  (debug op arg ... last)
  (begin
    (writeln (list (quote op) arg ... last))
    (op arg ... last)))

;;; 
;;; grouping plugin mechanism
;;; 

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

(define (get-grouping g-lst t)
  (or (findf (lambda (gr)
               (let ((open? (Grouping-open? gr)))
                 (open? t))) g-lst)
      (error "get-grouping: not found" t)))

;; Lazily turns group/End ranges into nested grouping constructions.
;;
;; Note that 's' must be complete, with matching numbers of opening
;; and closing tokens. We are currently not providing a way to suspend
;; and resume the streaming, and to put more data into the stream,
;; which would be useful for incremental operation. We could quite
;; easily refactor to allow for that, as we already internally
;; maintain explicit grouping state.
(define (group-stream s)
  ;; type:: grouping type (Grouping)
  ;; st:: grouping state (any)
  (struct Grp (type st) #:transparent)

  ;; grp:: current grouping (Grp or #f)
  ;; outer:: outer state (St or #f)
  (struct St (grp outer) #:transparent)

  (let next ((st #f) (s s))
    (lazy ;; even laziness
     (let-values (((h t) (tseq-get s)))
       ;;(writeln (list 'h h 't t 'st st))
       (if (not h)
           (let ((outer (and st (St-outer st))))
             (when outer ;; xxx do not check before 'flush'
               ((Grouping-eof (Grp-type outer)) (Grp-st outer)))
             s)
           (cond
            ((Begin? h)
             (let* ((g-type (get-grouping g-lst h))
                    (g-st ((Grouping-new g-type)))
                    (grp (Grp g-type g-st)))
               (next (St grp st) t)))
            ((End? h)
             (begin
               (unless st
                 (error "unopened grouping" h))
               (let* ((grp (St-grp st))
                      (g-type (Grp-type grp))
                      (end (Grouping-end g-type))
                      (g-st (Grp-st grp))
                      (g-tok (end g-st))
                      (outer (St-outer st)))
                 (if outer
                     (let ((name (Grouping-name g-type))
                           (st outer))
                       (let* ((outer (St-outer st))
                              (grp (St-grp st))
                              (g-type (Grp-type grp))
                              (g-st (Grp-st grp))
                              (accept (Grouping-accept g-type))
                              (n-g-st (accept g-st g-tok name))
                              (n-grp (Grp g-type n-g-st))
                              (n-st (St n-grp outer)))
                         (next n-st t)))
                     (tseq-cons g-tok (next #f t))))))
            (else
             (if st
                 (let* ((grp (St-grp st))
                        (g-type (Grp-type grp))
                        (put (Grouping-put g-type))
                        (g-st (put (Grp-st grp) h)))
                   (next (St (Grp g-type g-st) (St-outer st)) t))
                 (tseq-cons h (next st t))))
            ))))))

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

(struct* FmtSt (
                cw ;; specified page width (integer, constant)
                w ;; page width (rational)
                outDoc ;; formatted document (tseq of Token)
                inDoc ;; unread input (tseq of Token)
                k ;; current column (integer)
                lvStack ;; nesting stack (stack of string)
                bt ;; backtracking state (if any; can be chained)
                ) #:transparent)

;; w:: page width (integer)
;; inDoc:: unread input (tseq of Token, optional)
(define* (new-FmtSt w (inDoc empty-tseq))
  (FmtSt w w empty-tseq inDoc 0 '("") #f))

;; Flushes buffered documents, committing decisions made thus far.
;; After this it is safe to consume all of 'outDoc'.
(define* (flush st) ;; FmtSt -> FmtSt
  (struct-copy FmtSt st (bt #f)))

(define (process-token st) ;; FmtSt -> FmtSt
  (let ((inDoc (FmtSt-inDoc st)))
    (if (tseq-empty? inDoc)
        st
        (let ((d (tseq-first inDoc))
              (inDoc (tseq-rest inDoc)))
          (let ((k (FmtSt-k st))
                (w (FmtSt-w st))
                (outDoc (FmtSt-outDoc st))
                (i (car (FmtSt-lvStack st))))
            (cond
             ((Nest? d)
              (struct-copy FmtSt st (inDoc inDoc)
                           (lvStack (margin (FmtSt-lvStack st) k (Nest-lv d)))))
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
                                           (tseq
                                            (Text "\n")
                                            (Text i))))))
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
                                (tseq-append l (tseq-cons (Width w) inDoc)))
                               (bt r-st)))))
             ((Width? d)
              (struct-copy FmtSt st
                           (w (Width-w d))
                           (inDoc inDoc)))
             ((Together? d)
              (struct-copy FmtSt st
                           (inDoc (tseq-append (Together-m d) inDoc))))
             (else (error "process-token: unexpected" d))
             ))))))

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
