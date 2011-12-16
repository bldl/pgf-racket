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

(define* nil NIL)
(define* nest NEST)
(define* text TEXT)
(define* line LINE)

(define* (concat x . rest) ;; right associative
  (if (null? rest) x
      (CONCAT x (apply concat rest))))

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
   ((UNION? d) (flatten (UNION-ldoc d)))))

(define* (layout d)
  (cond
   ((Nil? d) "")
   ((Text? d) (string-append (Text-s d)
                             (layout (Text-doc d))))
   ((Line? d) (string-append "\n"
                             (make-string (Line-n d) #\space)
                             (layout (Line-doc d))))))

(struct Be (i doc))

(define (best w k x)
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
                  (be w k (cons (Be i (UNION-rdoc d)) z))))))))

(define (better w k x y)
  (if (fits (- w k) x) x y))

(define (fits w d)
  (cond
   ((< w 0) #f)
   ((Nil? d) #t)
   ((Text? d) (fits (- w (string-length (Text-s d))) (Text-doc d)))
   ((Line? d) #t)))

(define* (pretty w x)
  (layout (best w 0 x)))
