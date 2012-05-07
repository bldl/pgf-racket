#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "token.rkt")
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

;; Default line break.
(define current-line (make-parameter (Line)))

(define weak-f (make-parameter default-strength))
(define medium-f (make-parameter default-strength))
(define strong-f (make-parameter default-strength))

(define (weak-sh cw i k)
  ((weak-f) cw i k))
  
(define (medium-sh cw i k)
  ((medium-f) cw i k))

(define (strong-sh cw i k)
  ((strong-f) cw i k))

(define union/default union)

;; Choice construct. (Strong, push all the way to margin.)
(define (union/strong l r)
  (union l r strong-sh))

;; Choice construct.
(define (union/medium l r)
  (union l r medium-sh))

;; Choice construct. (Weak.)
(define (union/weak l r)
  (union l r weak-sh))

;; Forced linebreak. Might have a suffix.
(define (br/current)
  (current-line))

;; Breakable space.
(define (sp/current)
  (union/default (Text " ") (current-line)))
(define (sp/strong)
  (union/strong (Text " ") (current-line)))
(define (sp/medium)
  (union/medium (Text " ") (current-line)))
(define (sp/weak)
  (union/weak (Text " ") (current-line)))

;; Breakable point.
(define (breakable/current)
  (union/default empty-stream (current-line)))
(define (breakable/strong)
  (union/strong empty-stream (current-line)))
(define (breakable/medium)
  (union/medium empty-stream (current-line)))
(define (breakable/weak)
  (union/weak empty-stream (current-line)))

(define-syntax-rule
  (in-cpp body ...)
  (parameterize ((current-line (cat " \\" (Line)))) body ...))

#|
(define-syntax-rule
  (weaken body ...)
  (parameterize ((current-strength weak)) body ...))

(define-syntax-rule
  (strengthen body ...)
  (parameterize ((current-strength strong)) body ...))
|#

;;; 
;;; Pretty printing utilities
;;; 

(define (ind x)
  (cat (indent 2) x dedent))

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
        empty-stream
        (apply cat (reverse lst)))))

;;; 
;;; C++ specific pretty printing
;;; 

(define (parens doc)
  (cat (Text "(") (breakable/strong) doc (breakable/strong) (Text ")")))

(define (semi) (Text ";"))

(define (bracket l x r)
  (group (cat l (indent 2) br x dedent br r) strong-sh))

(define (cpp-if c t (e #f))
   (cat
    (in-cpp (cat (Text "#if") (sp/medium) c)) (br/current)
    t (br/current)
    (and e (cat (Text "#else") (br/current)
                   e (br/current)))
    (Text "#endif")))

(define (c-block doc)
  (cat
   (nest 2 (cat
            (Text "{") (Line)
            doc)) (Line)
   (Text "}")))

(define (stack/2-ln doc)
  (sep-by (stream (Line) (Line)) doc))

;; Puts opening "{" on a separate line (following any 'pre').
(define (indented-block #:pre (pre #f)
                        #:body (body #f)
                        #:body-elems (body-elems '())
                        #:post (post #f))
  (cat
   pre (Line)
   (Text "{")
   (if body
       (ind-cat (Line) body)
       (and (not (null? body-elems))
            (ind-cat (Line) (stack/2-ln body-elems))))
   (Line) (Text "}") post))

(define (c-struct name members)
  (let ((pre (cat (Text "struct") (sp/strong) name)))
    (indented-block #:pre pre
                    #:body-elems members
                    #:post (Text ";"))))

(define (c-int n)
  (Text (number->string n)))

(define (maybe-parens yes? doc)
  (if yes?
      (cat (Text "(") doc (Text ")"))
      doc))

(define (infix op)
  (cat (sp/strong) (Text op) (sp/weak)))

(define (infix/medium op)
  (cat (sp/strong) (Text op) (sp/medium)))

(define (infix/br op)
  (cat (Text " ") (Text op) (br/current)))

(define (c-add-expr exprs ctx)
  (let ((outer? (eq? ctx 'outer)))
    (let ((sep ((if outer? infix infix/medium) "+")))
      (maybe-parens
       (not outer?)
       (sep-by sep exprs)))))

(define (c-if-expr/fill c t e ctx)
  (maybe-parens (eq? ctx 'inner)
                (cat c (infix "?") t (infix ":") e)))

(define (c-if-expr c t e ctx)
  (maybe-parens (eq? ctx 'inner)
                (group (cat c (infix/br "?") t (infix/br ":") e))))

(define (c-call-expr n args)
  (let* ((xs (add-between args (cat (Text ",") (Line))))
         (x (apply cat xs)))
    (cat n (breakable/medium) (Text "(")
            align (group x strong-sh) dedent (Text ")"))))

(define (c-qual-type-inst n args)
  (let* ((x (group (sep-by (cat "," (Line)) args) strong-sh)))
    (cat n (breakable/weak) (Text "{")
            align x dedent (Text "}"))))

(define (c-expr-stmt expr)
  (cat expr (Text ";")))

;; Chooses formatting for the second block according to how the
;; formatting of the first block ends up fitting.
(define (two-block pre body-1 mid (body-2 #f))
  (cat
   (and pre (ind pre))
   (union/default
    (cat (Text " {")
         (and body-1 (ind-cat (Line) body-1))
         (Line) (Text "}")
         (and body-2
              (cat (sp/medium) mid (sp/medium) (Text "{")
                   (ind-cat (Line) body-2)
                   (Line) (Text "}"))))
    (cat (Line) (Text "{")
         (and body-1 (ind-cat (Line) body-1))
         (Line) (Text "}")
         (and body-2
              (cat (Line) mid (Line) (Text "{")
                   (ind-cat (Line) body-2)
                   (Line) (Text "}")))))))

(define (c-if-stmt c t-lst (e-lst '()))
  (two-block
   (cat (Text "if (") c (Text ")"))
   (and (not (null? t-lst)) (stack t-lst))
   (Text "else")
   (and (not (null? e-lst)) (stack e-lst))))

(define (c-func modifs rtype name params stmts #:doc (doc-s #f))
  (cat
   (and doc-s (cat (c-block-comment doc-s) (Line)))
   (ind-cat (and (not (null? modifs))
                 (apply cat
                        (map (lambda (modif)
                               (cat modif (sp/medium))) modifs)))
            (and rtype (cat rtype (sp/medium)))
            name
            (if (null? params)
                (Text "()")
                (bracket "("
                         (apply cat
                                (add-between params
                                             (cat (Text ",") (Line))))
                         ")")))
   (Line)
   (Text "{")
   (and (not (null? stmts))
        (ind-cat (Line)
                 (stack stmts)))
   (Line) (Text "}")))

(define (c-line-comment s)
  (cat (Text "// ") (nest/str "// " (fillwords s))))

(define (c-block-comment s)
  (cat (Text "/** ") align (fillwords s) dedent (Text " */")))

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
    (Text
     (string-append pfx
                    (random-string/readable (random/from-range min-len max-len)
                                   (cons underscore lower-lst))))))

(define (random-funname #:min (min-len 10)
                        #:max (max-len 15))
  (Text
   (random-string/readable (random/from-range min-len max-len)
                  (cons underscore (cons underscore lower-lst)))))

(define (random-cpp-varname #:min (min-len 10)
                            #:max (max-len 15))
  (Text
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
    (Text
     (string-append pfx
                    (random-string/readable
                     (random/from-range min-len max-len) lst)))))

(define (random-qual (count (+ 1 (random 2))))
  (times/cat count
             (cat (random-namespace-name) (Text "::") (breakable/weak))))

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
     (Text
      (string-append pfx
                     (random-string/readable
                      (random/from-range min-len max-len) lst)))
     (and args-ok (< (random (+ 8 depth)) 3)
          (let* ((n (random/from-range 1 4))
                 (args (times/sep/cat n
                                      (random-typename (+ depth 1) #:ref #t)
                                      (cat (Text ",") (Line)))))
            (bracket "<" args ">"))))))

(define (random-cpp-expr (depth 1))
  (define (f/br s1 doc s2)
    (cat (Text s1) (breakable/strong) doc (breakable/strong) (Text s2)))
  (define outer? (eqv? depth 1))
  (in-cpp
   (random-case/scored
     (int 5 (Text (number->string (random 1000))))
     (var 4 (random-cpp-varname))
     (defined 2 (f/br "defined(" (random-cpp-varname) ")"))
     (not 2 (if outer?
                (cat "!" (random-cpp-expr (+ depth 1)))
                (cat "(!" (random-cpp-expr (+ depth 1)) ")")))
     (and (- 7 depth)
          (cat "("
             (times/sep/cat
              (random/from-range 2 4)
              (random-cpp-expr (+ depth 1))
              (cat (sp/strong) (Text "&&") (sp/weak)))
             ")"))
     )))

(define (random-cpp-var-decl)
  (in-cpp
   (ind-cat (Text "#define") (sp/strong)
            (random-cpp-varname) (sp/weak)
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
  (ind-cat (and (eq? ctx 'tl) (cat (Text "static") (sp/medium)))
           (random-typename) (sp/medium) (random-varname) (semi)))

(define (random-typedef)
  (ind-cat (Text "typedef") (sp/medium)
           (random-typename #:ref #t) (sp/medium)
           (random-typename #:args-ok #f) (Text ";")))

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
   (add (- 5 depth)
        (let ((l (random-expr (+ depth 1) 'inner))
              (r-lst
               (let ((n (random/from-range 1 3)))
                 (times/list n (random-expr (+ depth 1) 'inner #:int? #t)))))
          (c-add-expr (cons l r-lst) ctx)))))

(define (random-stmt (depth 1))
  (random-case/scored
   (comment 1 (c-line-comment (random-sentence)))
   (var 4 (random-vardecl 'local))
   (return 2 (Text "return;"))
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
              (list (Text "static"))
              '()) ;; modifiers
          (Text "void") ;; return type
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
    (apply cat
           (add-between
            (times/list n (random-decl 'tl 1))
            (cat (Line) (Line))))))

;;; 
;;; Test data
;;; 

(define d-lst
  (let* (
         (true-1 (Text "true"))
         (break-1 (Text "break;"))
         (continue-1 (Text "continue;"))
         (if-1 (c-if-stmt true-1 (list break-1) (list continue-1)))
         ;;(cpp-1 (cpp-if-else "1" break-1 continue-1))
         )
    (list
     ;;(cons "type instantiation" (c-qual-type-inst (Text "T") (list (Text "1") (Text "2") (Text "3") (Text "4"))))
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
     ;;(cons "empty struct" (c-struct (Text "EmptyStruct") (list)))
     ;;(cons "struct with one member" (c-struct (Text "SmallStruct") (list (random-vardecl 'member))))
     ;; (cons "nothing" empty-stream)
     ;; (cons "break statement" break-1)
     ;; (cons "nested if statement with complex condition" (c-if-stmt (fillwords "1 && 2 && 1==2 && defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__") if-1))
     ;; (cons "#if-else" (cpp-if-else "defined(__FOO__) || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__ || !defined(__BAR__) || __FOOBAR__ || !__BAZ__" (Text "return 1;") (Text "return 2;")))
     ;; (cons "nested CPP" (c-if-stmt true-1 cpp-1))
     )))

(define w-lst '(5 10 15 25 35 45 55 65 75))

(define (force-sh m e)
  (lambda (cw i k)
    (let ((i (string-length i)))
      (* cw (expt (+ 1 (* m i)) (- e))))))

(define sh-lst
  (list
   (cons "default" (thunk (weak-f default-strength)
                          (medium-f default-strength)
                          (strong-f default-strength)))
   (cons "forced" (thunk (weak-f (force-sh .3 .3))
                         (medium-f (force-sh .15 .15))
                         (strong-f default-strength)))
   (cons "harder" (thunk (weak-f (force-sh .4 .4))
                         (medium-f (force-sh .2 .2))
                         (strong-f default-strength)))
   ))

;;; 
;;; Test runner
;;; 

(define (test-doc w t d sh-t sh-f)
  (printfln "// ~a (w=~a, sh=~a)" t w sh-t)
  ;;(for ((d d)) (write d))
  (displayln (width-divider w))
  (sh-f)
  (pgf-println w d)
  (displayln "// ----------"))

(define (main)
  (displayln "// -*- c++ -*-")
  (for* ((w (reverse w-lst))
         (d d-lst)
         (sh sh-lst))
        (test-doc w (car d) (cdr d) (car sh) (cdr sh))))
      
(main)
