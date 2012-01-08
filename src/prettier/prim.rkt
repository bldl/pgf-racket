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

;; xxx could have our macro create these also
(define (NIL? doc) (NIL_? (force/rec doc)))
(define (CONCAT? doc) (CONCAT_? (force/rec doc)))
(define (NEST? doc) (NEST_? (force/rec doc)))
(define (TEXT? doc) (TEXT_? (force/rec doc)))
(define (LINE? doc) (LINE_? (force/rec doc)))
(define (UNION? doc) (UNION_? (force/rec doc)))

;; xxx could have our macro create these also
(define (CONCAT-ldoc doc) (CONCAT_-ldoc (force/rec doc)))
(define (CONCAT-rdoc doc) (CONCAT_-rdoc (force/rec doc)))
(define (NEST-n doc) (NEST_-n (force/rec doc)))
(define (NEST-doc doc) (NEST_-doc (force/rec doc)))
(define (TEXT-s doc) (TEXT_-s (force/rec doc)))
(define (UNION-ldoc doc) (UNION_-ldoc (force/rec doc)))
(define (UNION-rdoc doc) (UNION_-rdoc (force/rec doc)))

(define* (to-sexp doc)
  (cond
   ((promise? doc) `(promise ,(to-sexp (force doc))))
   ((NIL_? doc) '(nil))
   ((CONCAT_? doc) `(concat ,(to-sexp (CONCAT_-ldoc doc))
                            ,(to-sexp (CONCAT_-rdoc doc))))
   ((NEST_? doc) `(nest ,(NEST_-n doc)
                        ,(to-sexp (NEST_-doc doc))))
   ((TEXT_? doc) `(text ,(TEXT_-s doc)))
   ((UNION_? doc) `(concat ,(to-sexp (UNION_-ldoc doc))
                           ,(to-sexp (UNION_-rdoc doc))))
   (else doc)))

(data Doc ((Nil) ;; -> Doc
           (Text s doc) ;; string, Doc -> Doc
           (Line n doc))) ;; integer, Doc -> Doc

(provide (rename-out (NIL nil)))
(provide (rename-out (NEST nest)))
(provide (rename-out (TEXT text)))
(provide (rename-out (LINE line)))

(define* (text/fun s)
  (TEXT s))

(define-syntax* concat
  (syntax-rules ()
    ((_ x) x)
    ((_ x y rest ...)
     (CONCAT x (concat y rest ...)))))

(provide (rename-out (UNION private-union)))

(define* (group x) (UNION (flatten x) x))

(define* (flatten d)
  ;; Even after forcing, a promise still remains a promise. Here we
  ;; redefine 'd' as a non-promise. (Assuming of course there were no
  ;; multiple promise wrappers.)
  (define d (force/rec d))
  (cond
   ((NIL_? d) d)
   ((CONCAT_? d) (CONCAT (flatten (CONCAT-ldoc d))
                         (flatten (CONCAT-rdoc d))))
   ((NEST_? d) (NEST (NEST-n d) (flatten (NEST-doc d))))
   ((TEXT_? d) d)
   ((LINE_? d) (TEXT " "))
   ((UNION_? d) (flatten (UNION-ldoc d)))
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
             (d (force/rec (Be-doc h)))
             (z (cdr lst)))
        (cond
         ((NIL_? d)
          (be w k z))
         ((CONCAT_? d)
          (be w k (cons (Be i (CONCAT-ldoc d))
                        (cons (Be i (CONCAT-rdoc d)) z))))
         ((NEST_? d)
          (be w k (cons (Be (+ i (NEST-n d)) (NEST-doc d)) z)))
         ((TEXT_? d)
          (let ((s (TEXT-s d)))
            (Text s (be w (+ k (string-length s)) z))))
         ((LINE_? d)
          (Line i (be w i z)))
         ((UNION_? d)
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
   ((Text? d) (fits (- w (string-length (Text-s d))) (Text-doc d)))
   ((Line? d) #t)
   (else (error "fits: unexpected" d))))

(define* (pretty w d)
  (layout (best w 0 d)))
