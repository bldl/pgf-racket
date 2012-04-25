#lang racket

(require "prim.rkt")
(require "util.rkt")

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
