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

(provide (rename-out (NIL nil)))
(provide (rename-out (TEXT text)))

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

(define* (group x) (private-union (flatten x) x))

(define* (flatten d)
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
(define* (best w k x)
  ;; Consume input until fully consumed.
  (let recur ((st (St (Nil) k (list (Be "" x)))))
    (let ((st (be w st)))
      (if (null? (St-lst st))
          (St-doc st)
          (recur st)))))

;; w:: page width (integer)
;; st:: state before choices (St)
;; Returns:: state after choices (St)
(define (be w st)
  ;; It would be more idiomatic to Scheme to use recursion here, but
  ;; we're instead using a loop as we don't want to rely on tail
  ;; recursion. Scheme has it, but this is to make porting easier to
  ;; languages that lack TCO.
  (let recur ((st st))
    (let ((lst (St-lst st)))
      (if (null? lst)
          st
          (let* ((k (St-k st))
                 (fd (St-doc st))
                 (h (car lst))
                 (i (Be-i h))
                 (d (Be-doc h))
                 (z (cdr lst)))
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
              (let ((s (TEXT-s d)))
                (recur (St (Concat fd (Text s))
                           (+ k (string-length s))
                           z))))
             ((LINE? d)
              ;; Note that we do not 'recur' further here. Rather we
              ;; return control to the caller. Here we lack the
              ;; context to know whether to proceed further or not.
              (St (Concat fd (Concat (Text (LINE-s d)) (Line i)))
                  (string-length i) z))
             ((UNION? d)
              ;; Note that here we call 'be' rather than invoking
              ;; 'recur', as 'recur' doesn't "return", it just
              ;; transfers control to the 'let'. Note, too, that we
              ;; start with a fresh empty document to make it possible
              ;; to compute its length easily.
              (let ((l-st (be w (St (Nil) k 
                                    (cons (Be i (UNION-ldoc d)) z)))))
                ;; In Wadler's algorithm the first argument of 'fits'
                ;; is computed as (- w k). This implementation takes a
                ;; fraction of available page width.
                (if (fits (- (* w (UNION-str d)) k) (St-doc l-st))
                    ;; Here, too, we return control, with the
                    ;; assumption that either a line break or the end
                    ;; of document has been encountered within the
                    ;; UNION.
                    (St (Concat fd (St-doc l-st))
                        (St-k l-st) (St-lst l-st))
                    ;; Left did not fit, we commit to right regardless
                    ;; of whether it fits.
                    (recur (St fd k (cons (Be i (UNION-rdoc d)) z))))))
             (else (error "be: unexpected" d))))))))

;; w:: remaining width (integer)
;; d:: formatted document whose first line to try fitting (St)
;; Returns:: whether fits (boolean)
(define (fits w d)
  (let recur ((w w)
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
   ((NIL? doc) '(nil))
   ((CONCAT? doc) `(concat ,(DOC-to-sexp (CONCAT-ldoc doc))
                           ,(DOC-to-sexp (CONCAT-rdoc doc))))
   ((NEST? doc) (let ((lv (NEST-lv doc))
                      (doc (DOC-to-sexp (NEST-doc doc))))
                  (cond
                   ((LvInc? lv) `(nest ,(LvInc-n lv) ,doc))
                   ((LvStr? lv) `(nest ,(LvStr-s lv) ,doc))
                   ((LvAbs? lv) `(nest/abs ,(LvAbs-n lv) ,doc))
                   (else (error "unexpected" lv)))))
   ((TEXT? doc) `(text ,(TEXT-s doc)))
   ((LINE? doc) `(line ,(LINE-s doc)))
   ((UNION? doc) `(union ,(UNION-str doc)
                         ,(DOC-to-sexp (UNION-ldoc doc))
                         ,(DOC-to-sexp (UNION-rdoc doc))))
   (else (error "DOC-to-sexp: unexpected" doc))))

(define* (Doc-to-sexp doc)
  (cond
   ((Nil? doc) '(nil))
   ((Text? doc) `(text ,(Text-s doc)))
   ((Line? doc) `(line ,(Line-s doc)))
   ((Concat? doc) `(concat ,(Doc-to-sexp (Concat-ldoc doc))
                           ,(Doc-to-sexp (Concat-rdoc doc))))
   (else (error "Doc-to-sexp: unexpected" doc))))
