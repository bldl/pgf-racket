#lang racket

(require "util.rkt")

;;; The pretty printer

(data DOC ((NIL) ;; -> DOC
           (CONCAT ldoc rdoc) ;; DOC, DOC -> DOC
           (NEST n doc) ;; integer, DOC -> DOC
           (TEXT s) ;; string -> DOC
           (LINE) ;; -> DOC
           (UNION ldoc rdoc))) ;; DOC, DOC -> DOC

(data Doc ((Nil) ;; -> Doc
           (Text s doc) ;; string, Doc -> Doc
           (Line n doc))) ;; integer, Doc -> Doc

(provide (rename-out (NIL nil)))
(provide (rename-out (NEST nest)))
(provide (rename-out (TEXT text)))
(provide (rename-out (LINE line)))

(define* concat
  (case-lambda
    ((x) x)
    ((x y . rest)
     (CONCAT x (apply concat y rest)))))

(provide (rename-out (UNION private-union)))

(define* (group x) (UNION (flatten x) x))

(define* (flatten d)
  (cond
   ((NIL? d) d)
   ((CONCAT? d) (CONCAT (flatten (CONCAT-ldoc d))
                        (flatten (CONCAT-rdoc d))))
   ((NEST? d) (NEST (NEST-n d) (flatten (NEST-doc d))))
   ((TEXT? d) d)
   ((LINE? d) (TEXT " "))
   ((UNION? d) (flatten (UNION-ldoc d)))
   (else (error "flatten: unexpected" d))))

(define* (layout d)
  (cond
   ((Nil? d) "")
   ((Text? d) (string-append (Text-s d)
                             (layout (Text-doc d))))
   ((Line? d) (string-append "\n"
                             (make-string (Line-n d) #\space)
                             (layout (Line-doc d))))
   (else (error "layout: unexpected" d))))

;;; 
;;; Formatting algorithm.
;;; 

(struct Be (i doc) #:transparent)

(define* (best w k x)
  (be w k (list (Be 0 x))))

(define (be w k lst)
  (if (null? lst) (Nil)
      (let* ((h (car lst))
             (i (Be-i h))
             (d (Be-doc h))
             (z (cdr lst)))
        (cond
         ((NIL? d)
          (be w k z))
         ((CONCAT? d)
          (be w k (cons (Be i (CONCAT-ldoc d))
                        (cons (Be i (CONCAT-rdoc d)) z))))
         ((NEST? d)
          (be w k (cons (Be (+ i (NEST-n d)) (NEST-doc d)) z)))
         ((TEXT? d)
          (let ((s (TEXT-s d)))
            (Text s (be w (+ k (string-length s)) z))))
         ((LINE? d)
          (Line i (be w i z)))
         ((UNION? d)
          (better w k
                  (be w k (cons (Be i (UNION-ldoc d)) z))
                  (be w k (cons (Be i (UNION-rdoc d)) z))))
         (else (error "be: unexpected" d))))))

(define (better w k x y)
  ;; Note that if 'x' fits, 'y' is not needed.
  (if (fits (- w k) x) x y))

(define (fits w d)
  (if (< w 0) #f
      (cond
       ((Nil? d) #t)
       ;; We don't want to force (Text-doc d) if (Text-s d) doesn't
       ;; fit.
       ((Text? d) (fits (- w (string-length (Text-s d))) (Text-doc d)))
       ;; It is quite important that we don't force (Line-doc d) in
       ;; this case.
       ((Line? d) #t)
       (else (error "fits: unexpected" d)))))

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
   ((NEST? doc) `(nest ,(NEST-n doc)
                       ,(DOC-to-sexp (NEST-doc doc))))
   ((TEXT? doc) `(text ,(TEXT-s doc)))
   ((UNION? doc) `(concat ,(DOC-to-sexp (UNION-ldoc doc))
                          ,(DOC-to-sexp (UNION-rdoc doc))))
   (else (error "DOC-to-sexp: unexpected" doc))))

(define* (Doc-to-sexp doc)
  (cond
   ((Nil? doc) '(nil))
   ((Text? doc) `(text ,(Text-s doc)
                       ,(Doc-to-sexp (Text-doc doc))))
   ((Line? doc) `(line ,(Line-n doc)
                       ,(Doc-to-sexp (Line-doc doc))))
   (else (error "Doc-to-sexp: unexpected" doc))))
