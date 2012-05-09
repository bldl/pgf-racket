#lang racket

#|
|#

(require "util.rkt")

(data* Lv ((LvInc n) ;; integer -> Lv
           (LvStr s) ;; string -> Lv
           (LvAbs n) ;; integer -> Lv
           (LvRel n) ;; integer -> Lv
           (LvPop))) ;; -> Lv

;; FmtEngine supports Nest, Text, Line, Union, and Width.
;;
;; Spacer supports Space, Anno, and /Anno tokens, dropping them or
;; translating them into something else. Otherwise it is token
;; agnostic.
(data* Token ((Nest lv) ;; Lv -> Token
              (Text s) ;; string -> Token
              (Line) ;; -> Token
              (Union l r sh) ;; stream, stream, function -> Token
              (Width w) ;; rational -> Token
              (Space s sh) ;; string, rational -> Token
              (Anno lst) ;; list of symbol -> Token
              (/Anno lst) ;; list of symbol -> Token
              (Group) ;; -> Token
              (End) ;; -> Token
              ))
