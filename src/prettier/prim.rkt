#lang racket

(require "util.rkt")

;;; The pretty printer

(data Lv ((LvInc n) ;; integer -> Lv
          (LvStr s) ;; string -> Lv
          (LvAbs n) ;; integer -> Lv
          (LvRel n))) ;; integer -> Lv

(data DOC ((NIL) ;; -> DOC
           (CONCAT ldoc rdoc) ;; DOC, DOC -> DOC
           (NEST lv doc) ;; Lv, DOC -> DOC
           (TEXT s) ;; string -> DOC
           (LINE s) ;; string -> DOC
           (UNION ldoc rdoc str))) ;; DOC, DOC, rational -> DOC

(data Doc ((Nil) ;; -> Doc
           (Text s) ;; string -> Doc
           (Line s) ;; string -> Doc
           (Concat ldoc rdoc))) ;; Doc, Doc -> Doc

;; f:: function to compute document (-> DOC)
;; doc:: cached non-LAZY document (DOC)
(struct LAZY DOC (f (doc #:mutable #:auto)) #:transparent)

;; x:: document (DOC)
;; Returns:: non-LAZY document (DOC)
(define (FORCE! x)
  (if (not (LAZY? x))
      x
      (aif d (LAZY-doc x)
           d
           (let ((d ((LAZY-f x))))
             (when (LAZY? d)
               (set! d (FORCE! d)))
             (set-LAZY-doc! x d)
             d))))

(provide (rename-out (NIL nil)))
(provide (rename-out (TEXT text)))
(provide (rename-out (LAZY susp)))

(define (UNION-str/val d)
  (let ((v (UNION-str d)))
    (if (procedure? v) (v) v)))

(define* (nest n doc)
  (NEST (LvInc n) doc))

(define* (nest/str s doc)
  (NEST (LvStr s) doc))

(define* (nest/abs n doc)
  (NEST (LvAbs n) doc))

(define* nest/0 (fix nest/abs 0))

(define* (nest/rel n doc)
  (NEST (LvRel n) doc))

(define* align (fix nest/rel 0))

(define* (line (hyphen ""))
  (LINE hyphen))

(define* concat
  (case-lambda
    (() (NIL))
    ((x) x)
    ((x y . rest)
     (CONCAT x (apply concat y rest)))))

;; str:: Eagerness to choose first fitting fragment. (rational)
(define* (private-union l r (str 1))
  (UNION l r str))

(define-syntax-rule*
  (private-union/lazy lexp rexp)
  (LAZY (thunk (private-union (LAZY (thunk lexp))
                              (LAZY (thunk rexp))))))

(define* (group x)
  (private-union (flatten x) x))

(define* (flatten d)
  (when (LAZY? d) (set! d (FORCE! d)))
  (cond
   ((NIL? d) d)
   ((CONCAT? d) (CONCAT (flatten (CONCAT-ldoc d))
                        (flatten (CONCAT-rdoc d))))
   ((NEST? d) (NEST (NEST-lv d) (flatten (NEST-doc d))))
   ((TEXT? d) d)
   ((LINE? d) (TEXT " "))
   ((UNION? d) (flatten (UNION-ldoc d)))
   (else (error "flatten: unexpected" d))))

;; k:: current column (integer)
;; s:: previous indentation string (string)
;; lv:: level specification (Lv)
(define (margin k s lv)
  (cond
   ((LvInc? lv) (let ((n (LvInc-n lv)))
                  (if (>= n 0)
                      (string-append s (make-string n #\space))
                      (let* ((len (string-length s))
                             (nlen (+ len n)))
                        (if (> nlen 0)
                            (substring s 0 nlen) "")))))
   ((LvStr? lv) (string-append s (LvStr-s lv)))
   ((LvAbs? lv) (let ((n (LvAbs-n lv)))
                  (if (> n 0)
                      (make-string n #\space) "")))
   ((LvRel? lv) (margin k s (LvAbs (+ k (LvRel-n lv)))))
   (else (error "margin: unexpected" lv))))

(define* (layout d)
  ;; Concat is used a lot in the generated documents, in a deeply
  ;; nesting manner, and hence there is potential for deep recursion
  ;; in traversing. We avoid that by using a loop.
  (let recur ((input (list d))
              (output ""))
    (shift-car
     output d input input
     (cond
      ((Nil? d) (recur input output))
      ((Text? d) (recur input (string-append output (Text-s d))))
      ((Line? d) (recur input (string-append output "\n" (Line-s d))))
      ((Concat? d) (recur (cons (Concat-ldoc d)
                                (cons (Concat-rdoc d) input)) output))
      (else (error "layout: unexpected" d))))))

;;; 
;;; Formatting algorithm.
;;; 

;; i:: nesting string (string)
;; doc:: document (DOC)
(struct Be (i doc) #:transparent)

;; doc:: formatted document (Doc)
;; k:: current column (integer)
;; lst:: unformatted documents with nesting levels (list of Be)
(struct St (doc k lst) #:transparent)

;; w:: page width (integer)
;; k:: current column (integer)
;; x:: remaining document (DOC)
;; Returns:: formatted document (Doc)
(define (best w k x)
  ;; In Racket we get faster code by avoiding the use of 'set!',
  ;; although this is not as Racketish.
  (let next ((st (St (Nil) k (list (Be "" x)))))
    (let-values (((dummy st) (be w st)))
      (if (null? (St-lst st))
          (St-doc st)
          (next st)))))

;; Formats input until reaches end-of-input or end-of-line. May return
;; earlier, to give the caller the chance to prune a choice that
;; doesn't fit; only in this case is the first return value false.
;;
;; w:: page width (integer)
;; st:: state before choices (St)
;; Returns:: return reason, state after choices (any, St)
(define (be w st)
  (let recur ((st st))
    (let ((lst (St-lst st)))
      (if (null? lst)
          (values 'eof st)
          (let* ((k (St-k st))
                 (fd (St-doc st))
                 (h (car lst))
                 (i (Be-i h))
                 (d (Be-doc h))
                 (z (cdr lst)))
            (when (LAZY? d) (set! d (FORCE! d)))
            (cond
             ((NIL? d)
              (recur (St fd k z)))
             ((CONCAT? d)
              (recur (St fd k (cons (Be i (CONCAT-ldoc d))
                                    (cons (Be i (CONCAT-rdoc d)) z)))))
             ((NEST? d)
              (recur (St fd k
                         (cons (Be (margin k i (NEST-lv d))
                                   (NEST-doc d)) z))))
             ((TEXT? d)
              ;; The Haskell version (I believe) notices immediately
              ;; if the length of a text exceeds available width. We
              ;; must make the same happen here. Typically text
              ;; consumes available width, meaning that there's a
              ;; possibility of running out.
              (let* ((s (TEXT-s d))
                     (l (string-length s)))
                (values #f (St (Concat fd (Text s)) (+ k l) z))))
             ((LINE? d)
              ;; Note that we do not 'recur' further here. Rather we
              ;; return control to the caller. Here we lack the
              ;; context to know whether to proceed further or not.
              (values 'eol
                      (St (Concat fd (Concat (Text (LINE-s d)) (Line i)))
                          (string-length i) z)))
             ((UNION? d)
              ;; We use recursion to explore the left choice.
              (let again ((l-st (St fd k 
                                    (cons (Be i (UNION-ldoc d)) z))))
                (let-values (((complete? l-st) (be w l-st)))
                  ;; In Wadler's algorithm the first argument of
                  ;; 'fits' is computed as (- w k). This
                  ;; implementation takes a fraction of available page
                  ;; width. We also do not have a separate 'fits', but
                  ;; rather we keep track of the column as we keep
                  ;; adding text so that we know immediately when we
                  ;; run out of space.
                  (cond
                   ((> (St-k l-st) (* w (UNION-str/val d)))
                    ;; Left did not fit, so discard it and continue.
                    ;; We've yet to get any more formatted document.
                    (recur (St fd k (cons (Be i (UNION-rdoc d)) z))))
                   ;; Fitting so far, but there's more.
                   ((not complete?) (again l-st))
                   ;; This left choice fits completely. If we've got a
                   ;; full line we'll return it so that it may be
                   ;; committed.
                   (else
                    (if (eq? 'eol complete?)
                        (values 'eol l-st)
                        (recur l-st)))))))
             (else (error "be: unexpected" d))))))))

;; rw:: remaining width (integer)
;; d:: formatted document whose first line to try fitting (Doc)
;; Returns:: whether fits (boolean)
(define (fits rw d)
  (let recur ((w rw)
              (lst (list d)))
    (cond
     ((< w 0) #f)
     ((null? lst) #t)
     (else
      (let ((d (car lst)))
        (cond
         ((Nil? d) (recur w (cdr lst)))
         ((Text? d) (recur (- w (string-length (Text-s d))) (cdr lst)))
         ((Line? d) #t)
         ((Concat? d) (recur w (cons (Concat-ldoc d)
                                     (cons (Concat-rdoc d) (cdr lst)))))
         (else (error "fits: unexpected" d))))))))

(define* (pretty w d)
  (layout (best w 0 d)))

;;; 
;;; Introspection utilities.
;;; 

(define* (DOC-to-sexp doc)
  (cond
   ((LAZY? doc) `(susp ,(aif x (LAZY-doc doc) (DOC-to-sexp x) #f)))
   ((NIL? doc) '(nil))
   ((CONCAT? doc) `(concat ,(DOC-to-sexp (CONCAT-ldoc doc))
                           ,(DOC-to-sexp (CONCAT-rdoc doc))))
   ((NEST? doc) (let ((lv (NEST-lv doc))
                      (doc (DOC-to-sexp (NEST-doc doc))))
                  (cond
                   ((LvInc? lv) `(nest ,(LvInc-n lv) ,doc))
                   ((LvStr? lv) `(nest/str ,(LvStr-s lv) ,doc))
                   ((LvAbs? lv) `(nest/abs ,(LvAbs-n lv) ,doc))
                   ((LvRel? lv) `(nest/rel ,(LvRel-n lv) ,doc))
                   (else (error "unexpected" lv)))))
   ((TEXT? doc) `(text ,(TEXT-s doc)))
   ((LINE? doc) `(line ,(LINE-s doc)))
   ((UNION? doc) `(union ,(UNION-str/val doc)
                         ,(DOC-to-sexp (UNION-ldoc doc))
                         ,(DOC-to-sexp (UNION-rdoc doc))))
   (else (error "DOC-to-sexp: unexpected" doc))))

(define* (DOC-to-string doc)
  (define (concat? x)
    (and (list? x) (not (null? x))
         (eq? (car x) 'concat)))
  (cond
   ((string? doc) doc)
   ((LAZY? doc) (aif x (LAZY-doc doc) (DOC-to-string x) '(susp)))
   ((NIL? doc) "")
   ((CONCAT? doc)
    (let ((l (DOC-to-string (CONCAT-ldoc doc)))
          (r (DOC-to-string (CONCAT-rdoc doc))))
      (if (and (string? l) (string? r))
          (string-append l r)
          (let ((lc (if (concat? l) (cdr l) (list l)))
                (rc (if (concat? r) (cdr r) (list r))))
            `(concat ,@lc ,@rc)))))
   ((NEST? doc)
    (let ((lv (NEST-lv doc))
          (doc (DOC-to-string (NEST-doc doc))))
      (cond
       ((LvInc? lv) `(nest ,(LvInc-n lv) ,doc))
       ((LvStr? lv) `(nest/str ,(LvStr-s lv) ,doc))
       ((LvAbs? lv) `(nest/abs ,(LvAbs-n lv) ,doc))
       ((LvRel? lv) `(nest/rel ,(LvRel-n lv) ,doc))
       (else (error "unexpected" lv)))))
   ((TEXT? doc) (TEXT-s doc))
   ((LINE? doc) (string-append (LINE-s doc) "\n"))
   ((UNION? doc) `(union ,(UNION-str/val doc)
                         ,(DOC-to-string (UNION-ldoc doc))
                         ,(DOC-to-string (UNION-rdoc doc))))
   (else (error "DOC-to-string: unexpected" doc))))

(define* (Doc-to-sexp doc)
  (cond
   ((Nil? doc) '(nil))
   ((Text? doc) `(text ,(Text-s doc)))
   ((Line? doc) `(line ,(Line-s doc)))
   ((Concat? doc) `(concat ,(Doc-to-sexp (Concat-ldoc doc))
                           ,(Doc-to-sexp (Concat-rdoc doc))))
   (else (error "Doc-to-sexp: unexpected" doc))))
