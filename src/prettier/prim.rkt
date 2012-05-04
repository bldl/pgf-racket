#lang racket

(require "token.rkt")
(require "util.rkt")

(define-syntax-rule
  (debug op arg ... last)
  (begin
    (writeln (list (quote op) arg ... last))
    (op arg ... last)))

(define (stream-put s t)
  (stream-append s (stream t)))

;;; 
;;; stack
;;; 

(define (spush st t)
  (cons t st))

(define (spop st)
  (values (cdr st) (car st)))

(define-syntax-rule
  (let-spop (h t st) e ...)
  (let ((h (car st))
        (t (cdr st)))
    e ...))

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
                outDoc ;; formatted document (stream of Token)
                inDoc ;; unread input (stream of Token)
                k ;; current column (integer)
                lvStack ;; nesting stack (stack of string)
                bt ;; backtracking state (if any; can be chained)
                ) #:transparent)

;; xxx try using define-sequence-syntax to define a way to 'for' over FmtSt using 'process-token', and naturally state should be made available for inspection and even modification

;; w:: page width (integer)
;; inDoc:: unread input (stream of Token, optional)
(define* (new-FmtSt w (inDoc empty-stream))
  (FmtSt w w empty-stream inDoc 0 '("") #f))

;; Flushes buffered documents, committing decisions made thus far.
;; After this it is safe to consume all of 'outDoc'.
(define* (flush st) ;; FmtSt -> FmtSt
  (struct-copy FmtSt st (bt #f)))

(define (process-token st) ;; FmtSt -> FmtSt
  (let ((inDoc (FmtSt-inDoc st)))
    (if (stream-empty? inDoc)
        st
        (let ((d (stream-first inDoc))
              (inDoc (stream-rest inDoc)))
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
                                   (k k) (outDoc (stream-put outDoc d)))))))
             ((Line? d)
              ;; A break always fits, and then we're committed, and
              ;; won't backtrack from here.
              (struct-copy FmtSt st (inDoc inDoc)
                           (k (string-length i))
                           (bt #f)
                           (outDoc
                            (stream-append outDoc
                                           (stream
                                            (Text "\n")
                                            (Text i))))))
             ((Union? d)
              ;; Pick left option, leave right for backtracking.
              (let ((l (Union-l d))
                    (r (Union-r d))
                    (sh (Union-sh d)))
                (let ((r-st
                       (struct-copy FmtSt st
                                    (inDoc (stream-append r inDoc)))))
                  (struct-copy FmtSt st
                               (w (* (FmtSt-cw st) sh))
                               (inDoc
                                (stream-append l (stream-cons (Width w) inDoc)))
                               (bt r-st)))))
             ((Width? d)
              (struct-copy FmtSt st
                           (w (Width-w d))
                           (inDoc inDoc)))
             (else (error "process-token: unexpected" d))
             ))))))

(define (FmtSt-eof? st) ;; FmtSt -> boolean
  (stream-empty? (FmtSt-inDoc st)))

;; Adds a stream to input.
(define* (FmtSt-write st s) ;; FmtSt, stream of Token -> St
  (struct-copy FmtSt st (inDoc (stream-append (FmtSt-inDoc st) s))))

;; Adds a token to input.
(define* (FmtSt-put st t) ;; FmtSt, Token -> St
  (FmtSt-write st (stream t)))

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
(define* (pgf-string/stream ts) ;; stream of Token -> string
  (apply string-append
         (for/list ((t (in-stream ts)))
                   (pgf-string/token t))))

(define* (pgf-string/st st)
  (pgf-string/stream
   (FmtSt-outDoc
    (process-tokens st))))

;; w:: page width
;; ts:: formatted document
;; Returns:: pretty-printed string
(define* (pgf-string w ts) ;; integer, stream -> string
  (pgf-string/st (new-FmtSt w ts)))

;; Clears output buffer by printing it all out.
(define* (pgf-print/st/buffered st (out (current-output-port)))
  (for ((t (FmtSt-outDoc st)))
       (display (pgf-string/token t) out))
  (struct-copy FmtSt st (outDoc empty-stream)))

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

(define* default-strength 1)

;; l:: left choice (stream of Token)
;; r:: right choice (stream of Token)
;; sh:: eagerness to choose first fitting fragment (rational)
;; Returns:: Token
(define* (private-union l r (sh default-strength))
  (Union l r sh))

;; Behaves lazily. Note the use of stateless iterators to avoid the
;; cost of creating a new closure for every iteration. This is
;; inspired by Lua's ipairs, although we require no invariant state.
;; http://www.lua.org/pil/7.3.html
(define* (private-flatten ts) ;; stream of Token -> stream of Token
  (if (stream-empty? ts)
      ts
      (let ((t (stream-first ts))
            (ts (stream-rest ts)))
        (cond
         ((Line? t)
          (stream-cons (Text " ") (private-flatten ts)))
         ((Union? t)
          (private-flatten (stream-append (Union-l t) ts)))
         (else
          (stream-cons t (private-flatten ts)))))))

(define* (private-group ts) ;; stream of Token -> Token
  (private-union (private-flatten ts) ts))
