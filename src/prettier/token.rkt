#lang racket

#|
|#

(require "util.rkt")

(data* Lv ((LvInc n) ;; integer -> Lv
           (LvStr s) ;; string -> Lv
           (LvAbs n) ;; integer -> Lv
           (LvRel n) ;; integer -> Lv
           (LvPop))) ;; -> Lv

;; Note that the precense of annotations means that 'struct-copy' will
;; not work for Tokens.
(data/anno* Token ((Nest lv) ;; Lv -> Token
                   (Text s) ;; string -> Token
                   (Line s) ;; string -> Token
                   (Union l r sh) ;; stream, stream, rational -> Token
                   (Width w))) ;; rational -> Token

(define* (anno t) ;; Token -> any
  (Token-anno t))

;; Careful with this as it's mutating. The idea is that you tag a
;; token with its semantics (as desired) after creating it, and then
;; leave the annotation well alone.
(define* (set-anno! t v) ;; Token, any -> Token (with side effects)
  (set-Token-anno! t v) t)

