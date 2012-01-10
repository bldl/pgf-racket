#lang racket

(require "lazy-data.rkt")
(require "util.rkt")

;;; The pretty printer

(lazy-data DOC ((NIL) ;; -> DOC
                (CONCAT (susp ldoc) (susp rdoc)) ;; DOC, DOC -> DOC
                (NEST n (susp doc)) ;; integer, DOC -> DOC
                (TEXT s) ;; string -> DOC
                (LINE) ;; -> DOC
                (UNION (susp ldoc) (susp rdoc)))) ;; DOC, DOC -> DOC

(lazy-data Doc ((Nil) ;; -> Doc
                (Text s (susp doc)) ;; string, Doc -> Doc
                (Line n (susp doc)))) ;; integer, Doc -> Doc

(provide (rename-out (NIL nil)))
(provide (rename-out (NEST nest)))
(provide (rename-out (TEXT text)))
(provide (rename-out (LINE line)))

(define-syntax* concat
  (syntax-rules ()
    ((_ x) x)
    ((_ x y rest ...)
     (CONCAT x (concat y rest ...)))))

(provide (rename-out (UNION private-union)))

(define* (group x) (UNION (flatten x) x))

(define* (text/fun s)
  (TEXT s))

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
                  (delay (be w k (cons (Be i (UNION-rdoc d)) z)))))
         (else (error "be: unexpected" d))))))

(define (better w k x y)
  ;; Note that if 'x' fits, 'y' is not needed.
  (if (fits (- w k) x) x (force y)))

(define (fits w d)
  (cond
   ((< w 0) #f)
   ((Nil? d) #t)
   ;; We don't want to force (Text-doc d) if (Text-s d) doesn't
   ;; fit.
   ((Text? d) (fits (- w (string-length (Text-s d))) (Text-doc d)))
   ;; It is quite important that we don't force (Line-doc d) in
   ;; this case.
   ((Line? d) #t)
   (else (error "fits: unexpected" d))))

(define* (pretty w d)
  (layout (best w 0 d)))

(define* (DOC-to-sexp doc)
  (cond
   ((promise? doc) `(promise ,(DOC-to-sexp (force doc))))
   ((NIL_? doc) '(nil))
   ((CONCAT_? doc) `(concat ,(DOC-to-sexp (CONCAT_-ldoc doc))
                            ,(DOC-to-sexp (CONCAT_-rdoc doc))))
   ((NEST_? doc) `(nest ,(NEST_-n doc)
                        ,(DOC-to-sexp (NEST_-doc doc))))
   ((TEXT_? doc) `(text ,(TEXT_-s doc)))
   ((LINE_? doc) '(line))
   ((UNION_? doc) `(concat ,(DOC-to-sexp (UNION_-ldoc doc))
                           ,(DOC-to-sexp (UNION_-rdoc doc))))
   (else (error "DOC-to-sexp: unexpected" doc))))

(define* (Doc-to-sexp doc)
  (cond
   ((promise? doc) `(promise ,(Doc-to-sexp (force doc))))
   ((Nil_? doc) '(nil))
   ((Text_? doc) `(text ,(Text_-s doc)
                        ,(Doc-to-sexp (Text_-doc doc))))
   ((Line_? doc) `(line ,(Line_-n doc)
                        ,(Doc-to-sexp (Line_-doc doc))))
   (else (error "Doc-to-sexp: unexpected" doc))))
