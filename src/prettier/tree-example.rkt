#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; Tree example

(data Tree ((Node s lst)))

(define (show-tree t)
  (let ((s (Node-s t))
        (ts (Node-lst t)))
    (group (concat (text s)
                   (nest (string-length s) (show-bracket ts))))))

(define (show-bracket ts)
  (if (null? ts)
      (nil)
      (concat (text "[")
              (nest 1 (show-trees ts))
              (text "]"))))

(define (show-trees lst)
  (let ((t (car lst))
        (ts (cdr lst)))
    (if (null? ts)
        (show-tree t)
        (concat (show-tree t)
                (text ",")
                (line)
                (show-trees ts)))))

(define (show-tree^ t)
  (let ((s (Node-s t))
        (ts (Node-lst t)))
    (concat (text s)
            (show-bracket^ ts))))

(define (show-bracket^ ts)
  (if (null? ts)
      (nil)
      (bracket "[" (show-trees^ ts) "]")))

;; The paper uses exactly the same implementation as for show-trees,
;; but that is probably not right.
(define (show-trees^ lst)
  (let ((t (car lst))
        (ts (cdr lst)))
    (if (null? ts)
        (show-tree^ t)
        (concat (show-tree^ t)
                (text ",")
                (line)
                (show-trees^ ts)))))

(define tree
  (Node "aaa" (list
               (Node "bbbbb" (list
                              (Node "ccc" empty)
                              (Node "dd" empty)
                              ))
               (Node "eee" empty)
               (Node "ffff" (list
                             (Node "gg" empty)
                             (Node "hhh" empty)
                             (Node "ii" empty)
                             ))
               )))

(define* (testtree w)
  (displayln (pretty w (show-tree tree))))

(define* (testtree^ w)
  (displayln (pretty w (show-tree^ tree))))
