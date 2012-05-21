#lang racket

(require "prim.rkt")
(require "token.rkt")
(require "util.rkt")

;;; 
;;; Introspection utilities.
;;; 

;; Prints the full document; i.e. expands all promises. Only the
;; primitive token types may appear. The document need not be
;; formatted, however.
(define* (tseq-to-sexp s)
  (map
   (lambda (t)
     (cond
      ((Text? t) `(text ,(Text-s t)))
      ((Line? t) '(br))
      ((Space? t) `(space ,(Space-s t)))
      ((Union? t) `(union ,(tseq-to-sexp (Union-l t))
                          ,(tseq-to-sexp (Union-r t))))
      ((Nest? t) (let ((lv (Nest-lv t)))
                  (cond
                   ((LvInc? lv) `(nest/inc ,(LvInc-n lv)))
                   ((LvStr? lv) `(nest/str ,(LvStr-s lv)))
                   ((LvAbs? lv) `(nest/abs ,(LvAbs-n lv)))
                   ((LvRel? lv) `(nest/rel ,(LvRel-n lv)))
                   ((LvPop? lv) '(/nest))
                   (else (error "tseq-to-sexp: unexpected level" lv)))))
      ((Begin? t) (let* ((g-type (Begin-grouping t))
                         (name (Grouping-name g-type)))
                    `(begin ,name)))
      ((End? t) (let ((g-type (End-grouping t)))
                  (if g-type `(end ,(Grouping-name g-type)) '(end))))
      ((SpaceT? t) `(space/t ,(SpaceT-s t)))
      ((Together? t) `(together ,(tseq-to-sexp (Together-m t))))
      (else (error "tseq-to-sexp: unexpected token" t))))
   (tseq->list (tseq-optimize s))))

#;
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

#;
(define* (Doc-to-sexp doc)
  (cond
   ((Nil? doc) '(nil))
   ((Text? doc) `(text ,(Text-s doc)))
   ((Line? doc) `(line ,(Line-s doc)))
   ((Concat? doc) `(concat ,(Doc-to-sexp (Concat-ldoc doc))
                           ,(Doc-to-sexp (Concat-rdoc doc))))
   (else (error "Doc-to-sexp: unexpected" doc))))
