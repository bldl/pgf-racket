#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "util.rkt")

;;; 
;;; Randomness utils
;;; 

(define (one-in-three?)
  (= (random 3) 0))

(define (one-in-two?)
  (= (random 2) 0))

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

(define (random-string/readable n alphabet #:capitalize? (cap #f))
  (let next ((n n) (lst '()) (was? #t))
    (if (> n 0)
        (let* ((c (random/from-list alphabet))
               (v (vowel/us? c))
               (ok (or v was?)))
          (if ok
              (let ((c (if (and cap (null? lst)) (char-upcase c) c)))
                (next (- n 1) (cons c lst) v))
              (next n lst was?)))
        (apply string (reverse lst)))))

(define (random-sentence)
  (let ((s (apply string-append
                  (add-between
                   (times/list (+ 3 (random 100))
                               (random-string/readable
                                (random/from-range 3 10)
                                lower-lst))
                   " "))))
    (string-set! s 0 (char-upcase (string-ref s 0)))
    (string-append s ".")))

;;; 
;;; Pretty printing configuration
;;; 

(define strong 1)
(define weak 4/5)

(define current-line (make-parameter (line)))
(define current-strength (make-parameter strong))

;; Choice construct. (Default strength.)
(define (union l r)
  (private-union l r (current-strength)))

;; Choice construct. (Strong, push all the way to margin.)
(define (union/strong l r)
  (private-union l r strong))

;; Choice construct. (Weak.)
(define (union/weak l r)
  (private-union l r weak))

;; Forced linebreak.
(define (nl)
  (current-line))

;; Non-breakable space.
(define (nbsp)
  (text " "))

;; Breakable space.
(define (sp)
  (union (text " ") (current-line)))
(define (sp/strong)
  (union/strong (text " ") (current-line)))
(define (sp/weak)
  (union/weak (text " ") (current-line)))

;; Breakable point.
(define (br)
  (union (nil) (current-line)))
(define (br/strong)
  (union/strong (nil) (current-line)))
(define (br/weak)
  (union/weak (nil) (current-line)))

(define-syntax-rule
  (in-cpp body ...)
  (parameterize ((current-line (line " \\"))) body ...))

(define-syntax-rule
  (weaken body ...)
  (parameterize ((current-strength weak)) body ...))

(define-syntax-rule
  (strengthen body ...)
  (parameterize ((current-strength strong)) body ...))

;;; 
;;; Pretty printing utilities
;;; 

(define (ind doc)
  (nest 2 doc))

(define (cat . lst)
  (apply concat (filter identity lst)))

(define (cat/str . args)
  (apply cat (map (lambda (x) (if (string? x) (text x) x)) args)))

(define (ind-cat . args)
  (ind (apply cat args)))

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

;;; 
;;; C++ specific pretty printing
;;; 

(define (parens doc)
  (concat (text "(") (br) doc (br) (text ")")))

(define (semi) (text ";"))

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

(define (stack/2-ln doc)
  (define (</> x y)
    (concat x (line) (line) y))
  (folddoc </> doc))

;; Puts opening "{" on a separate line (following any 'pre').
(define (indented-block #:pre (pre #f)
                        #:body (body #f)
                        #:body-elems (body-elems '())
                        #:post (post #f))
  (cat
   pre (line)
   (text "{")
   (if body
       (ind-cat (line) body)
       (and (not (null? body-elems))
            (ind-cat (line) (stack/2-ln body-elems))))
   (line) (text "}") post))

(define (c-struct name members)
  (let ((pre (cat (text "struct") (sp) name)))
    (indented-block #:pre pre
                    #:body-elems members
                    #:post (text ";"))))

(define (c-int n)
  (text (number->string n)))

(define (maybe-parens yes? doc)
  (if yes?
      (concat (text "(") doc (text ")"))
      doc))

(define (c-add-expr exprs ctx)
  (let ((sep (private-union (text " + ")
                            (concat (text " +") (nl)))))
    (maybe-parens
     (not (eq? ctx 'outer))
     (folddoc (lambda (x y)
                (concat x sep y)) exprs))))

(define (infix op)
  (private-union
   (text (format " ~a " op))
   (concat (text (format " ~a" op)) (nl))))

(define (c-if-expr c t e ctx)
  (maybe-parens (eq? ctx 'inner)
                (concat c (infix "?") t (infix ":") e)))

(define (c-call-expr n args)
  (let* ((xs (add-between args (concat (text ",") (line))))
         (x (apply concat xs)))
    (concat n (br) (text "(")
            (align (group x)) (text ")"))))

(define (c-qual-type-inst n args)
  (let* ((xs (add-between args (concat (text ",") (line))))
         (x (apply concat xs)))
    (concat n (br) (text "{")
            (align (group x)) (text "}"))))

(define (c-expr-stmt expr)
  (concat expr (text ";")))

;; Tries to put opening "{" on the same line as 'pre'.
(define (indented-block/same pre body)
  (private-union
   (concat
    (ind-cat
     pre (text "{1")
     (and body (concat (line) body)))
    (line) (text "}1"))
   (ind-cat
    pre (text "{2")
    (and body (concat (line) body (line)))
    (text "}2"))))

;; Chooses formatting for the second block according to how the
;; formatting of the first block ends up fitting.
(define (two-block pre body-1 mid (body-2 #f))
  (cat
   (and pre (ind pre))
   (private-union
    (cat (text " {")
         (and body-1 (ind-cat (line) body-1))
         (line) (text "}")
         (and body-2
              (cat (sp) mid (sp) (text "{")
                   (ind-cat (line) body-2)
                   (line) (text "}"))))
    (cat (line) (text "{")
         (and body-1 (ind-cat (line) body-1))
         (line) (text "}")
         (and body-2
              (cat (line) mid (line) (text "{")
                   (ind-cat (line) body-2)
                   (line) (text "}")))))))

(define (c-if-stmt c t-lst (e-lst '()))
  (two-block
   (concat (text "if (") c (text ")"))
   (and (not (null? t-lst)) (stack t-lst))
   (text "else")
   (and (not (null? e-lst)) (stack e-lst))))

(define (c-func modifs rtype name params stmts #:doc (doc-s #f))
  (cat
   (and doc-s (concat (c-block-comment doc-s) (line)))
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
                 (stack stmts)))
   (line) (text "}")))

(define (c-line-comment s)
  (concat (text "// ") (nest/str "// " (fillwords s))))

(define (c-block-comment s)
  (concat (text "/** ") (align (fillwords s)) (text " */")))

;;; 
;;; C++ test data generator
;;; 

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

(define (random-cpp-expr (depth 1))
  (define (f/br s1 doc s2)
    (concat (text s1) (br) doc (br) (text s2)))
  (define outer? (eqv? depth 1))
  (in-cpp
   (random-case/scored
     (int 5 (text (number->string (random 1000))))
     (var 4 (random-cpp-varname))
     (defined 2 (f/br "defined(" (random-cpp-varname) ")"))
     (not 2 (if outer?
                (cat/str "!" (random-cpp-expr (+ depth 1)))
                (cat/str "(!" (random-cpp-expr (+ depth 1)) ")")))
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
            (random-cpp-expr))))

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
  (c-struct (random-typename #:args-ok #f)
            (let ((n (random/from-range 0 6)))
              (times/list n (random-decl 'member (+ depth 1))))))

(define (random-call (depth 1))
  (let* ((n (random/from-range 0 3))
         (args (times/list n (random-expr (+ depth 1) 'outer #:int? #t))))
    (c-call-expr (random-funname #:min 5 #:max 10) args)))

(define (random-qual-type-inst)
  (let* ((t (random-typename #:min 4 #:max 5 #:args-ok #f))
         (n (random/from-range 1 15))
         (args (times/list n (c-int (random 1000)))))
    (c-qual-type-inst t args)))

(define (random-expr (depth 1) (ctx 'outer) #:int? (int? #f))
  (random-case/scored
   (call (- 5 depth) (random-call depth))
   (type-inst 2 (random-qual-type-inst))
   (var-ref 5 (random-varname))
   (int-lit (if int? 5 0) (c-int (random 1000)))
   (if (- 4 depth) (c-if-expr (random-expr (+ depth 1) 'inner)
                              (random-expr (+ depth 1) 'inner)
                              (random-expr (+ depth 1) 'inner) ctx))
   (add (- 3 depth)
        (let ((l (random-expr (+ depth 1) 'inner))
              (r-lst
               (let ((n (random/from-range 1 4)))
                 (times/list n (random-expr (+ depth 1) 'inner #:int? #t)))))
          (c-add-expr (cons l r-lst) ctx)))))

(define (random-stmt (depth 1))
  (random-case/scored
   (comment 1 (c-line-comment (random-sentence)))
   (var 4 (random-vardecl 'local))
   (return 2 (text "return;"))
   (expr 4 (c-expr-stmt (random-call)))
   (if (- 5 depth)
       (c-if-stmt (random-expr #:int? #t)
                  (times/list (random 5) (random-stmt (+ depth 1)))
                  (if (one-in-two?)
                      (times/list (random 5) (random-stmt (+ depth 1)))
                      '())))
   (cpp-if (- 2 depth) (cpp-if (random-cpp-expr)
                               (random-stmt (+ depth 1))
                               (and (one-in-three?)
                                    (random-stmt (+ depth 1)))))))

(define (random-func ctx)
  (c-func (if (eq? ctx 'tl)
              (list (text "static"))
              '()) ;; modifiers
          (text "void") ;; return type
          (random-funname) ;; function name
          '() ;; parameters
          (let ((n (random/from-range 0 6)))
            (times/list n (random-stmt)))
          #:doc (and (one-in-three?) (random-sentence))
          ))

(define (random-decl ctx depth)
  (random-case/scored
    (struct (- 5 depth) (random-struct depth))
    (var 4 (random-vardecl ctx))
    (fun 4 (random-func ctx))
    (typedef 3 (random-typedef))
    (cpp-var (if (eq? ctx 'tl) 2 0) (random-cpp-var-decl))
    (cpp-if (- 1 depth) (cpp-if (random-cpp-expr)
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
         (if-1 (c-if-stmt true-1 (list break-1) (list continue-1)))
         ;;(cpp-1 (cpp-if-else "1" break-1 continue-1))
         )
    (list
     ;;(cons "function declaration" (random-func 'tl))
     ;;(cons "line comment" (c-line-comment "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit."))
     ;;(cons "block comment" (c-block-comment "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit."))
     ;;(cons "if statement without else" (c-if-stmt true-1 (list break-1)))
     ;;(cons "if statement with else" if-1)
     ;;(cons "statement" (random-stmt))
     ;;(cons "expression" (random-expr))
     ;;(cons "variable #define" (random-cpp-var-decl))
     ;;(cons "CPP expression" (random-cpp-expr))
     (cons "random compilation unit" (random-compilation-unit))
     ;;(cons "top-level declaration" (random-decl 'tl 1))
     ;;(cons "empty struct" (c-struct (text "EmptyStruct") (list)))
     ;;(cons "struct with one member" (c-struct (text "SmallStruct") (list (random-vardecl 'member))))
     ;; (cons "nothing" (nil))
     ;; (cons "break statement" break-1)
     ;; (cons "nested if statement with complex condition" (c-if-stmt (fillwords "1 && 2 && 1==2 && defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__") if-1))
     ;; (cons "#if-else" (cpp-if-else "defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__" (text "return 1;") (text "return 2;")))
     ;; (cons "nested CPP" (c-if-stmt true-1 cpp-1))
     )))

(define w-lst '(5 10 15 25 35 45 55 65 75))

;;; 
;;; Test runner
;;; 

(define (test-doc w t d)
  (printfln "// ~a (w=~a)" t w)
  ;;(writeln (DOC-to-sexp d))
  ;;(writeln (DOC-to-string d))
  (displayln (width-divider w))
  (displayln (pretty w d))
  (displayln "// ----------"))

(define (main)
  (for* ((w (reverse w-lst))
         (d d-lst))
        (test-doc w (car d) (cdr d))))
      
(main)
