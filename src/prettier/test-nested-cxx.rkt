#lang racket

#|

Here we want to try varying strengths depending on the nesting level
of expressions, while we otherwise use the default strength.

|#

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

;; Strength function maker.
(define (force-sh m e)
  (lambda (cw i k)
    (let ((i (string-length i)))
      (* cw (expt (+ 1 (* m i)) (- e))))))

(define weak (force-sh .3 .3))
(define medium (force-sh .15 .15))

(define (depth-sh d)
  ;;(writeln `(depth ,d))
  (cond
   ((= d 1) weak)
   ((= d 2) medium)
   (else default-strength)))

;; Default line break.
(define current-line (make-parameter (Line)))

;; Forced linebreak. Might have a suffix.
(define (br/current)
  (current-line))

;; Breakable space.
(define (sp/current (sh default-strength))
  (union (Text " ") (current-line) sh))

;; Breakable point.
(define (breakable/current (sh default-strength))
  (union empty-stream (current-line) sh))

(define-syntax-rule
  (in-cpp body ...)
  (parameterize ((current-line (cat " \\" (Line)))) body ...))

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
  (cat (Text "(") doc (Text ")")))

(define (semi) (Text ";"))

(define (bracket l x r)
  (group (cat l (indent 2) br x dedent br r)))

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
  (let ((pre (cat (Text "struct") (sp/current) name)))
    (indented-block #:pre pre
                    #:body-elems members
                    #:post (Text ";"))))

(define (c-int n)
  (Text (number->string n)))

(define (maybe-parens yes? doc)
  (if yes?
      (cat (Text "(") doc (Text ")"))
      doc))

(define (infix op depth)
  (cat (sp/current) (Text op) (sp/current (depth-sh depth))))

(define (infix/br op)
  (cat (Text " ") (Text op) (br/current)))

(define (c-bin-expr op-txt exprs ctx depth)
  (let ((outer? (eq? ctx 'outer)))
    (let ((sep (infix op-txt depth)))
      (maybe-parens
       (not outer?)
       (sep-by sep exprs)))))

(define (c-if-expr c t e ctx)
  (maybe-parens (eq? ctx 'inner)
                (group (cat c (infix/br "?") t (infix/br ":") e))))

(define (c-call-expr n args)
  (let* ((xs (add-between args (cat (Text ",") (Line))))
         (x (apply cat xs)))
    (cat n (breakable/current) (Text "(")
            align (group x) dedent (Text ")"))))

(define (c-qual-type-inst n args)
  (let* ((x (group (sep-by (cat "," (Line)) args))))
    (cat n (breakable/current) (Text "{")
            align x dedent (Text "}"))))

(define (c-expr-stmt expr)
  (cat expr (Text ";")))

;; Chooses formatting for the second block according to how the
;; formatting of the first block ends up fitting.
(define (two-block pre body-1 mid (body-2 #f))
  (cat
   (and pre (ind pre))
   (union
    (cat (Text " {")
         (and body-1 (ind-cat (Line) body-1))
         (Line) (Text "}")
         (and body-2
              (cat (sp/current medium) mid (sp/current medium) (Text "{")
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
                               (cat modif (sp/current medium))) modifs)))
            (and rtype (cat rtype (sp/current medium)))
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
             (cat (random-namespace-name) (Text "::") (breakable/current))))

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
  (ind-cat (random-typename) (sp/current medium) (random-varname)
           (sp/current medium) "=" (sp/current weak)
           (random-expr) (semi)))

(define (random-struct depth)
  (c-struct (random-typename #:args-ok #f)
            (let ((n (random/from-range 0 6)))
              (times/list n (random-decl 'member (+ depth 1))))))

(define (random-call (depth 1))
  (let* ((n (random/from-range 0 3))
         (args (times/list n (random-expr (+ depth 1) 'outer))))
    (c-call-expr (random-funname #:min 5 #:max 10) args)))

(define (random-qual-type-inst)
  (let* ((t (random-typename #:min 4 #:max 5 #:args-ok #f))
         (n (random/from-range 1 15))
         (args (times/list n (c-int (random 1000)))))
    (c-qual-type-inst t args)))

(define (random-bin-expr depth ctx)
  (let ((op (random/from-list '("+" "-")))
        (l (random-expr (+ depth 1) 'inner))
        (r-lst
         (let ((n (random/from-range 1 4)))
           (times/list n (random-expr (+ depth 1) 'inner)))))
    (c-bin-expr op (cons l r-lst) ctx depth)))

(define (random-expr (depth 1) (ctx 'outer))
  (random-case/scored
   (var-ref 4 (random-varname))
   (int-lit 5 (c-int (random 1000)))
   (if (- 4 depth) (c-if-expr (random-expr (+ depth 1) 'inner)
                              (random-expr (+ depth 1) 'inner)
                              (random-expr (+ depth 1) 'inner) ctx))
   (bin (- 5 depth) (random-bin-expr depth ctx))))

(define (random-return)
  (ind-cat "return" (sp/current medium) (random-expr) (semi)))

(define (random-expr-stmt) ;; assignment
  (ind-cat (random-varname) (sp/current medium) "="
           (sp/current weak)
           ;; 'outer due to lowest associativity
           (random-expr 1 'outer) (semi)))

(define (random-stmt (depth 1))
  (random-case/scored
   (var 3 (random-vardecl 'local))
   (return 2 (random-return))
   (expr 3 (random-expr-stmt))
   (if (- 5 depth)
       (c-if-stmt (random-expr)
                  (times/list (random 5) (random-stmt (+ depth 1)))
                  (if (one-in-two?)
                      (times/list (random 5) (random-stmt (+ depth 1)))
                      '())))))

(define (random-func ctx)
  (c-func (if (eq? ctx 'tl)
              (list (Text "static"))
              '()) ;; modifiers
          (Text "int") ;; return type
          (random-funname) ;; function name
          '() ;; parameters
          (append
           (let ((n (random/from-range 0 6)))
             (times/list n (random-stmt)))
           (list (random-return)))
          #:doc (and (one-in-three?) (random-sentence))
          ))

(define (random-decl ctx depth)
  (random-case/scored
    (fun 4 (random-func ctx))))

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
  (list
   (cons "random compilation unit" (random-compilation-unit))
   ))

(define w-lst '(5 10 15 25 35 45 55 65 75))

(define sh-lst
  (list
   (cons "default" (thunk (void)))
   ))

;;; 
;;; Test runner
;;; 

(define (test-doc w t d sh-t sh-f)
  (printfln "// ~a (w=~a, sh=~a)" t w sh-t)
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
