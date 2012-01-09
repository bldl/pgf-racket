#lang racket

(require "lazy-data.rkt")
(require "profile.rkt")
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
  ;; Even after forcing, a promise still remains a promise. Here we
  ;; redefine 'd' as a non-promise. If there are multiple 'lazy'
  ;; wrappers, a single force will force them all.
  (let ((d (force d)))
    (cond
     ((NIL_? d) d)
     ((CONCAT_? d) (CONCAT (flatten (CONCAT_-ldoc d))
                           (flatten (CONCAT_-rdoc d))))
     ((NEST_? d) (NEST (NEST_-n d) (flatten (NEST_-doc d))))
     ((TEXT_? d) d)
     ((LINE_? d) (TEXT " "))
     ((UNION_? d) (flatten (UNION_-ldoc d)))
     (else (error "flatten: unexpected" d)))))

(define* (layout d)
  (let ((d (force d)))
    (cond
     ((Nil_? d) "")
     ((Text_? d) (string-append (Text_-s d)
                                (layout (Text_-doc d))))
     ((Line_? d) (string-append "\n"
                                (make-string (Line_-n d) #\space)
                                (layout (Line_-doc d))))
     (else (error "layout: unexpected" d)))))

(struct Be (i doc) #:transparent)

(define* (best w k x)
  (be w k (list (Be 0 x))))

(define (be w k lst)
  (if (null? lst) (Nil)
      (let* ((h (car lst))
             (i (Be-i h))
             (d (force (Be-doc h)))
             (z (cdr lst)))
        (cond
         ((NIL_? d)
          (be w k z))
         ((CONCAT_? d)
          (be w k (cons (Be i (CONCAT_-ldoc d))
                        (cons (Be i (CONCAT_-rdoc d)) z))))
         ((NEST_? d)
          (be w k (cons (Be (+ i (NEST_-n d)) (NEST_-doc d)) z)))
         ((TEXT_? d)
          (let ((s (TEXT_-s d)))
            (Text s (be w (+ k (string-length s)) z))))
         ((LINE_? d)
          (Line i (be w i z)))
         ((UNION_? d)
          (better w k
                  (be w k (cons (Be i (UNION_-ldoc d)) z))
                  (delay (be w k (cons (Be i (UNION_-rdoc d)) z)))))
         (else (error "be: unexpected" d))))))

(define (better w k x y)
  ;; Note that if 'x' fits, 'y' is not needed.
  (if (fits (- w k) x) x (force y)))

(define (fits w d)
  (if (< w 0) #f
      (let ((d (force d)))
        (cond
         ((Nil_? d) #t)
         ;; We don't want to force (Text-doc d) if (Text-s d) doesn't
         ;; fit.
         ((Text_? d) (fits (- w (string-length (Text_-s d))) (Text_-doc d)))
         ;; It is quite important that we don't force (Line-doc d) in
         ;; this case.
         ((Line_? d) #t)
         (else (error "fits: unexpected" d))))))

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
   ((UNION_? doc) `(concat ,(DOC-to-sexp (UNION_-ldoc doc))
                           ,(DOC-to-sexp (UNION_-rdoc doc))))
   (else (error "DOC-to-sexp: unexpected" doc))))

(define* (Doc-to-sexp doc)
  (cond
   ((promise? doc) `(promise ,(Doc-to-sexp (force doc))))
   ((Nil_? doc) '(nil))
   ((Text_? doc) `(text ,(Text_-s doc)
                        ,(Doc-to-sexp (Text_-doc doc))))
   ((Line_? doc) `(line ,(Line-n doc)
                        ,(Doc-to-sexp (Line_-doc doc))))
   (else (error "Doc-to-sexp: unexpected" doc))))
