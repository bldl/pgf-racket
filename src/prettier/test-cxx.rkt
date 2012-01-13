#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; 
;;; C++ specific pretty printing
;;; 

(define current-line (make-parameter (line)))

(define (sp)
  (private-union (text " ") (current-line)))

(define (br)
  (private-union (nil) (current-line)))

(define-syntax-rule
  (in-cpp body ...)
  (parameterize ((current-line (line " \\"))) body ...))

(define (ind doc)
  (nest 2 doc))

(define (cat . lst)
  (apply concat (filter identity lst)))

(define (parens doc)
  (concat (text "(") (br) doc (br) (text ")")))

#;
(define (cpp-directive s)
  (nest 2
        (folddoc
         (lambda (x y)
           ;; This actually breaks the rule we have for UNION, but
           ;; line breaks are special.
           (concat x (private-union (text " ") (line " \\")) y))
         (map text (words s)))))

#;
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
   (ind (concat (text "if") (sp) (parens c))) (sp)
   (c-block t)
   (and e
        (concat (ind (concat (text "else"))) (sp) (c-block e)))))

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

(define (vowel? c)
  (memv c (list #\a #\e #\i #\o #\u #\y
                #\A #\E #\I #\O #\U #\Y)))

(define (random/from-list lst)
  (list-ref lst (random (length lst))))

(define (random/from-range a b)
  (+ a (random (- b a))))

(define (random-string n lst)
  (apply string (for/list ((i (in-range n))) (random/from-list lst))))

(define (random-string/readable n alphabet)
  (let next ((n n) (lst '()) (was? #t))
    (if (> n 0)
        (let* ((c (random/from-list alphabet))
               (v (vowel? c))
               (ok (or v was?)))
          (if ok
              (next (- n 1) (cons c lst) v)
              (next n lst was?)))
        (apply string (reverse lst)))))

(define (random-varname #:min (min 5)
                        #:max (max 10)
                        #:member? (member? #f))
  (let* ((pfx (if member? "m_" ""))
         (pfx-len (string-length pfx))
         (min-len (- min pfx-len))
         (max-len (- max pfx-len)))
    (text
     (string-append pfx
                    (random-string/readable (random/from-range min-len max-len)
                                   (cons underscore lower-lst))))))

(define (random-funname #:min (min-len 10)
                        #:max (max-len 15))
  (text
   (random-string/readable (random/from-range min-len max-len)
                  (cons underscore lower-lst))))

(define (random-cpp-varname #:min (min-len 10)
                            #:max (max-len 15))
  (text
   (random-string/readable (random/from-range min-len max-len)
                  (cons underscore upper-lst))))

(define (random-typename #:min (min 5)
                         #:max (max 15)
                         #:args-ok (args-ok #t))
  (let* ((pfx "T")
         (pfx-len (string-length pfx))
         (min-len (- min pfx-len))
         (max-len (- max pfx-len))
         (lst (cons underscore lower-lst)))
    (cat 
     (text
      (string-append pfx
                     (random-string/readable
                      (random/from-range min-len max-len) lst)))
     (and args-ok (= (random 3) 0)
          (let* ((n (random/from-range 1 4))
                 (args (times/sep/cat n (random-typename)
                                      (concat (text ",") (line)))))
            (bracket "<" args ">"))))))

(define (semi) (text ";"))

(define (ind-cat . args)
  (ind (apply cat args)))

(define (bracket/br l x r)
  (group (concat (text l)
                 (nest 2 (concat (br) x))
                 (br)
                 (text r))))

(define (random-cpp-expr)
  (in-cpp
   (random-case
    (int var defined not parens and)
    (generate
     (int with (text (number->string (random 1000))))
     (var with (random-cpp-varname))
     (defined with (bracket/br "defined(" (random-cpp-varname) ")"))
     (not with (bracket/br "!(" (random-cpp-expr) ")"))
     (parens with (bracket/br "(" (random-cpp-expr) ")"))
     (and with (bracket/br "("
                           (times/sep/cat
                            (random/from-range 2 4)
                            (random-cpp-expr)
                            (concat (sp) (text "&&") (sp)))
                           ")"))
     ))))

(define (random-cpp-var-decl)
  (in-cpp
   (ind-cat (text "#define") (sp)
            (random-cpp-varname) (sp)
            (random-cpp-expr))))

(define-syntax random-case
  (syntax-rules (generate with)
    ((_ (names ...) (generate (name with expr) ...))
     (let ((kind (random/from-list '(names ...))))
       (cond
        ((eq? kind (quote name))
         expr)
        ...
        (else (error "unsupported" kind)))))))

(define (random-vardecl ctx)
  (ind-cat (and (eq? ctx 'tl) (concat (text "static") (sp)))
           (random-typename) (sp) (random-varname) (semi)))

(define (random-typedef)
  (ind-cat (text "typedef") (sp)
           (random-typename) (sp)
           (random-typename #:args-ok #f) (text ";")))

;; 'struct' and function to be supported
(define (random-decl ctx)
  (random-case
   (var typedef cpp-var)
   (generate
    (var with (random-vardecl ctx))
    (typedef with (random-typedef))
    (cpp-var with (random-cpp-var-decl)))))

(define (random-compilation-unit)
  (let ((n (random/from-range 7 12)))
    (apply concat
           (add-between
            (times/list n (random-decl 'tl))
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
         ;;(cpp-1 (cpp-if-else "1" break-1 continue-1))
         )
    (list
     (cons "random compilation unit" (random-compilation-unit))
     ;; (cons "nothing" (nil))
     ;; (cons "break statement" break-1)
     ;; (cons "if statement" if-1)
     ;; (cons "nested if statement with complex condition" (c-if (fillwords "1 && 2 && 1==2 && defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__") if-1))
     ;; (cons "#if-else" (cpp-if-else "defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__" (text "return 1;") (text "return 2;")))
     ;; (cons "nested CPP" (c-if true-1 cpp-1))
     )))

(define w-lst '(5 10 15 25 35 45 55 65 75))

;;; 
;;; Test runner
;;; 

(define (test-doc w t d)
  (printfln "~a (w=~a)" t w)
  (newline)
  ;;(writeln (DOC-to-sexp d)) (newline)
  (displayln (pretty w d))
  (displayln "----------"))

(define (main)
  (for* ((w w-lst)
         (d d-lst))
        (test-doc w (car d) (cdr d))))
      
(main)
