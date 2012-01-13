#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; 
;;; C++ specific pretty printing
;;; 

(define (sp)
  (private-union (text " ") (line)))

(define (br)
  (private-union (nil) (line)))

(define (ind doc)
  (nest 2 doc))

(define (cat . lst)
  (apply concat (filter identity lst)))

(define (parens doc)
  (concat (text "(") (br) doc (br) (text ")")))

(define (cpp-directive s)
  (nest 2
        (folddoc
         (lambda (x y)
           ;; This actually breaks the rule we have for UNION, but
           ;; line breaks are special.
           (concat x (private-union (text " ") (line " \\")) y))
         (map text (words s)))))

(define (cpp-if-else c-s t e)
  (concat (cpp-directive (string-append "#if " c-s)) (line)
          t (line)
          (text "#else") (line)
          e (line)
          (text "#endif")))

(define (c-block doc)
  (concat
   (nest 2 (concat
            (text "{") (line)
            doc)) (line)
   (text "}")))

(define (c-if c t (e #f))
  (cat
   (concat (ind (concat (text "if") (sp) (parens c))) (sp))
   (c-block t)))

;;; 
;;; C++ test data generator
;;; 

(define-syntax-rule
  (times/list n expr)
  (for/list ((i (in-range n))) expr))

(define-syntax-rule
  (times/sep/cat count elem-expr sep-expr)
  (let ((lst '()))
    (let recur ((n count)
                (last #f))
      (when (> n 0)
        (when last
          (let ((sep sep-expr))
            (when sep
              (set! lst (cons sep lst)))))
        (let ((elem elem-expr))
          (when elem
            (set! lst (cons elem lst)))
          (recur (- n 1) elem))))
    (if (null? lst)
        (nil)
        (apply concat (reverse lst)))))

(define ascii-lst
  (for/list ((i (in-range 128)))
            (integer->char i)))
(define lower-lst (filter char-lower-case? ascii-lst))
(define upper-lst (filter char-upper-case? ascii-lst))
(define alpha-lst (append lower-lst upper-lst))
(define underscore #\_)

(define (random/from-list lst)
  (list-ref lst (random (length lst))))

(define (random/from-range a b)
  (+ a (random (- b a))))

(define (random/string n lst)
  (apply string (for/list ((i (in-range n))) (random/from-list lst))))

(define (random-varname #:min (min 5)
                        #:max (max 10)
                        #:member? (member? #f))
  (let* ((pfx (if member? "m_" ""))
         (pfx-len (string-length pfx))
         (min-len (- min pfx-len))
         (max-len (- max pfx-len)))
    (text
     (string-append pfx
                    (random/string (random/from-range min-len max-len)
                                   (cons underscore lower-lst))))))

(define (random-funname #:min (min-len 10)
                        #:max (max-len 15))
  (text
   (random/string (random/from-range min-len max-len)
                  (cons underscore lower-lst))))

(define (random-typename #:min (min 5)
                         #:max (max 15)
                         #:args-ok (args-ok #t))
  (let* ((pfx "T")
         (pfx-len (string-length pfx))
         (min-len (- min pfx-len))
         (max-len (- max pfx-len))
         (lst (cons underscore alpha-lst)))
    (text
     (string-append pfx
                    (random/string
                     (random/from-range min-len max-len) lst)))))

(define (semi) (text ";"))

(define (ind-cat . args)
  (ind (apply cat args)))

(define (random-vardecl #:global? (global? #f))
  (ind-cat (and global? (concat (text "static") (sp)))
           (random-typename) (sp) (random-varname) (semi)))

(define (random-typedef)
  (ind-cat (text "typedef") (sp)
           (random-typename) (sp)
           (random-typename #:args-ok #f) (text ";")))

(define-syntax random-case
  (syntax-rules (generate with)
    ((_ (names ...) (generate (name with expr) ...))
     (let ((kind (random/from-list '(names ...))))
       (cond
        ((eq? kind (quote name))
         expr)
        ...
        (else (error "unsupported" kind)))))))

(define (random-decl)
  (random-case
   (var typedef)
   (generate
    (var with (random-vardecl #:global? #t))
    (typedef with (random-typedef)))))

#;
(define (random-decl)
  (let ((kind (random/from-list '(var typedef))))
    (cond
     ((eq? kind 'var)
      (random-vardecl #:global? #t))
     ((eq? kind 'typedef)
      (random-typedef))
     )))

(define (random-compilation-unit)
  (let ((n (random/from-range 7 12)))
    (apply concat
           (add-between
            (times/list n (random-decl))
            (concat (line) (line))))))

;;; 
;;; Test data
;;; 

(define d-lst
  (let* (
         (true-1 (text "true"))
         (break-1 (text "break;"))
         (continue-1 (text "continue;"))
         (if-1 (c-if true-1 break-1))
         (cpp-1 (cpp-if-else "1" break-1 continue-1))
         )
    (list
     (cons "random compilation unit" (random-compilation-unit))
     (cons "random declaration" (random-decl))
     (cons "nothing" (nil))
     (cons "break statement" break-1)
     (cons "if statement" if-1)
     (cons "nested if statement with complex condition" (c-if (fillwords "1 && 2 && 1==2 && defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__") if-1))
     (cons "#if-else" (cpp-if-else "defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__" (text "return 1;") (text "return 2;")))
     (cons "nested CPP" (c-if true-1 cpp-1))
     )))

(define w-lst '(5 10 15 25 35 45 55))

;;; 
;;; Test runner
;;; 

(define (test-doc w t d)
  (printfln "~a (w=~a)" t w)
  (newline)
  (writeln (DOC-to-sexp d)) (newline)
  (displayln (pretty w d))
  (displayln "----------"))

(define (main)
  (for* ((w w-lst)
         (d d-lst))
        (test-doc w (car d) (cdr d))))
      
(main)
