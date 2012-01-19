#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; Test code

(define lorem-ipsum-paragraph "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur. Donec ut libero sed arcu vehicula ultricies a non tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean ut gravida lorem. Ut turpis felis, pulvinar a semper sed, adipiscing id dolor. Pellentesque auctor nisi id magna consequat sagittis. Curabitur dapibus enim sit amet elit pharetra tincidunt feugiat nisl imperdiet. Ut convallis libero in urna ultrices accumsan. Donec sed odio eros. Donec viverra mi quis quam pulvinar at malesuada arcu rhoncus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In rutrum accumsan ultricies. Mauris vitae nisi at sem facilisis semper ac in est.")

(define (<+/>* r x y)
  (concat x (private-union (text " ") (line) r) y))

(define (fillwords* r s)
  (folddoc (fix <+/>* r) (map text (words s))))

(define (to-doc x)
  (cond
   ((string? x) (text x))
   ((number? x) (text (number->string x)))
   (else x)))

(define map/to-doc (fix map to-doc))

(define (cat lst)
  (apply concat (filter identity lst)))

(define cat/to-doc (compose cat map/to-doc))

(define (cat/sep sep lst)
  (apply concat (add-between (filter identity lst) sep)))

(define (parens l x r)
  (concat (text "(") x (text ")")))

(define nbsp (text " "))
(define ln (line))
(define sp (private-union nbsp ln))
(define ssp (private-union nbsp ln 4/5))

(define (op/0 name . args)
  (parens "(" (concat (text name) sp (align (cat/sep sp args))) ")"))

(define (op/1 name . args)
  (parens "(" (concat (text name) sp (align (cat/sep ssp args))) ")"))

(define (op/2 name . args)
  (parens "(" (concat (text name) sp (align (apply group* args))) ")"))

(define d-lst
  (let* ((a (text "aaaaa"))
         (b (text "bbbbb"))
         (c (text "ccccc"))
         (d (text "ddddd"))
         (d-1 (folddoc private-union
                       (list (concat a sp b sp c sp d)
                             (concat a sp b ln c sp d)
                             (concat a ln b ln c ln d))))
         )
    (list ;;(cons "full" (fillwords lorem-ipsum-paragraph))
          ;;(cons "3/4" (fillwords* 3/4 lorem-ipsum-paragraph))
          ;;(cons "group*" (apply group* (map/to-doc '(1 2 3 4 5))))
          ;;(cons "better" d-1)
          (cons "normal" (op/0 "+" (text "foo") (text "bar")
                             (op/2 "+" (text "1")
                                   (text "2") (text "3")
                                   (text "4") (text "5"))))
          (cons "cool" (op/1 "+" (text "foo") (text "bar")
                             (op/2 "+" (text "1")
                                   (text "2") (text "3")
                                   (text "4") (text "5"))))
          )))

(define w-lst '(2 7 15 17 25 50))

(define (test-doc w t d)
  (printfln "// ~a (w=~a)" t w)
  (writeln (DOC-to-string d))
  (displayln (width-divider w))
  (displayln (pretty w d))
  (displayln "// ----------"))

(define (main)
  (for* ((w (reverse w-lst))
         (d d-lst))
        (test-doc w (car d) (cdr d))))
      
(main)
