#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; 
;;; C++ specific pretty printing
;;; 

(define current-line (make-parameter (line)))

(define (nl)
  (current-line))

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

(define (semi) (text ";"))

(define (ind-cat . args)
  (ind (apply cat args)))

(define (bracket/br l x r)
  (group (concat (text l)
                 (nest 2 (concat (br) x))
                 (br)
                 (text r))))

(define (cpp-if c t (e #f))
   (cat
    (in-cpp (concat (text "#if") (sp) c)) (nl)
    t (nl)
    (and e (concat (text "#else") (nl)
                   e (nl)))
    (text "#endif")))

(define (c-block doc)
  (concat
   (nest 2 (concat
            (text "{") (line)
            doc)) (line)
   (text "}")))

(define (indented-block pre body)
  (private-union
   (concat
    (ind-cat
     pre (text "{")
     (and body (concat (line) body)))
    (line) (text "}"))
   (ind-cat
    pre (text "{")
    (and body (concat (line) body (line)))
    (text "}"))))

(define (c-struct name members)
  (let ((pre (concat (text "struct") (sp)))
        (body (and (not (null? members))
                   (apply concat
                          (add-between members
                                       (concat (line) (line)))))))
    (concat (indented-block pre body) (text ";"))))

(define (c-if c t (e #f))
  (cat
   (ind (concat (text "if") (sp) (parens c))) (sp)
   (c-block t)
   (and e
        (concat (ind (concat (text "else"))) (sp) (c-block e)))))

(define (c-func modifs rtype name params stmts)
  (cat
   (ind-cat (and (not (null? modifs))
                 (apply concat
                        (map (lambda (modif)
                               (concat modif (sp))) modifs)))
            (and rtype (concat rtype (sp)))
            name
            (if (null? params)
                (text "()")
                (bracket/br "("
                            (apply concat
                                   (add-between params
                                                (concat (text ",") (line))))
                            ")")))
   (line)
   (text "{")
   (and (not (null? stmts))
        (ind-cat (line)
                 (apply stack stmts)))
   (line) (text "}")))

;;; 
;;; C++ test data generator
;;; 

(define (one-in-three?)
  (= (random 3) 0))

(define-syntax-rule
  (times/list n expr)
  (for/list ((i (in-range n))) expr))

(define-syntax-rule
  (times/cat count elem-expr)
  (apply cat (times/list count elem-expr)))

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
(define (underscore? c)
  (eqv? c underscore))
(define (vowel/us? c)
  (or (vowel? c) (underscore? c)))

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
               (v (vowel/us? c))
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
                  (cons underscore (cons underscore lower-lst)))))

(define (random-cpp-varname #:min (min-len 10)
                            #:max (max-len 15))
  (text
   (random-string/readable (random/from-range min-len max-len)
                  (cons underscore
                        (cons underscore upper-lst)))))

(define (random-namespace-name
         #:min (min 5)
         #:max (max 8))
  (let* ((pfx "N")
         (pfx-len (string-length pfx))
         (min-len (- min pfx-len))
         (max-len (- max pfx-len))
         (lst (cons underscore lower-lst)))
    (text
     (string-append pfx
                    (random-string/readable
                     (random/from-range min-len max-len) lst)))))

(define (random-qual (count (+ 1 (random 2))))
  (times/cat count
             (concat (random-namespace-name) (text "::") (br))))

(define (random-typename (depth 1)
                         #:min (min 5)
                         #:max (max 15)
                         #:ref (ref #f)
                         #:args-ok (args-ok #t))
  (let* ((pfx "T")
         (pfx-len (string-length pfx))
         (min-len (- min pfx-len))
         (max-len (- max pfx-len))
         (lst (cons underscore lower-lst)))
    (cat
     (and ref (one-in-three?)
          (random-qual))
     (text
      (string-append pfx
                     (random-string/readable
                      (random/from-range min-len max-len) lst)))
     (and args-ok (< (random (+ 8 depth)) 3)
          (let* ((n (random/from-range 1 4))
                 (args (times/sep/cat n
                                      (random-typename (+ depth 1) #:ref #t)
                                      (concat (text ",") (line)))))
            (bracket "<" args ">"))))))

(define (cat/str . args)
  (apply cat (map (lambda (x) (if (string? x) (text x) x)) args)))

(define (random-cpp-expr (depth 1))
  (define (f/br s1 doc s2)
    (concat (text s1) (br) doc (br) (text s2)))
  (in-cpp
   (random-case/scored
     (int 5 (text (number->string (random 1000))))
     (var 4 (random-cpp-varname))
     (defined 2 (f/br "defined(" (random-cpp-varname) ")"))
     (not 2 (cat/str "!(" (random-cpp-expr (+ depth 1)) ")"))
     (and (- 7 depth)
          (cat/str "("
             (times/sep/cat
              (random/from-range 2 4)
              (random-cpp-expr (+ depth 1))
              (concat (sp) (text "&&") (sp)))
             ")"))
     )))

(define (random-cpp-var-decl)
  (in-cpp
   (ind-cat (text "#define") (sp)
            (random-cpp-varname) (sp)
            (random-cpp-expr 1))))

(define-syntax random-case/scored
  (syntax-rules ()
    ((_ (name score-expr gen-expr) ...)
     (let* ((c-lst (list (list (quote name)
                               score-expr
                               (delay gen-expr)) ...))
            (sum 0)
            (c-map
             (for/hasheq ((i c-lst))
                         (let ((sc (second i)))
                           (when (> sc 0)
                             (set! sum (+ sum sc))))
                         (values (first i)
                                 (cons sum (third i)))))
            (k (random sum)))
       (cond
        ((let ((elem (hash-ref c-map (quote name))))
           (and (< k (car elem))
                (force (cdr elem)))))
        ...
        (else (error "mismatch" k)))))))

(define (random-vardecl ctx)
  (ind-cat (and (eq? ctx 'tl) (concat (text "static") (sp)))
           (random-typename) (sp) (random-varname) (semi)))

(define (random-typedef)
  (ind-cat (text "typedef") (sp)
           (random-typename #:ref #t) (sp)
           (random-typename #:args-ok #f) (text ";")))

(define (random-struct depth)
  (c-struct (random-typename)
            (let ((n (random/from-range 0 6)))
              (times/list n (random-decl 'member (+ depth 1))))))

;; xxx random expression to be supported (one of variable ref, function call, trinary expression, qualified type instantiation, '+' expression, and on the right hand side of a '+' expression integer literals allowed as well)

;; xxx random statement to be supported (one of 'return', cpp-if, 'if' statement, expression statement, (local) var declaration)
(define (random-func ctx)
  (c-func (if (eq? ctx 'tl)
              (list (text "static"))
              '()) ;; modifiers
          (text "void") ;; return type
          (random-funname) ;; function name
          '() ;; parameters
          '() ;; body statements xxx
          ))

(define (random-decl ctx depth)
  (random-case/scored
    (struct (- 5 depth) (random-struct depth))
    (var 4 (random-vardecl ctx))
    (fun 4 (random-func ctx))
    (typedef 3 (random-typedef))
    (cpp-var (if (eq? ctx 'tl) 2 0) (random-cpp-var-decl))
    (cpp-if (- 1 depth) (cpp-if (random-cpp-expr depth)
                                (random-decl ctx depth)
                                (and (one-in-three?)
                                     (random-decl ctx depth))))))

(define (random-compilation-unit)
  (let ((n (random/from-range 7 12)))
    (apply concat
           (add-between
            (times/list n (random-decl 'tl 1))
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
     (cons "function declaration" (random-func 'tl))
     (cons "CPP expression" (random-cpp-expr))
     ;;(cons "random compilation unit" (random-compilation-unit))
     (cons "top-level declaration" (random-decl 'tl 1))
     (cons "variable #define" (random-cpp-var-decl))
     (cons "empty struct" (c-struct (text "EmptyStruct") (list)))
     (cons "struct with one member"
           (c-struct (text "SmallStruct") (list (random-vardecl 'member))))
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
