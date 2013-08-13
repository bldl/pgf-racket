#lang racket

#|

Implements a banker's deque, as per Okasaki. Uses Racket lists as the
stream type. The invariant maintained for [lenf f lenr r] is (and (<=
lenf (+ (* (dq-factor) lenr) 1)) (<= lenr (+ (* (dq-factor) lenf)
1))), for some constant '(dq-factor)', in order to maintain balance.
'f' means "front" and 'r' means "rear". The algorithm supports every
deque operation in O(1) amortized time.

TODO Earlier we used Clojure's lazy sequences as the stream type.
Check that our choice of using Racket lists is also appropriate to
maintain time/space characteristics of the algorithm.

|#

(require "util.rkt")

(define* dq-factor (make-parameter 7))

(struct Deque (lenf f lenr r) #:transparent)

;; An empty double-ended queue.
(define* dq-empty
  (Deque 0 '() 0 '()))

(define* dq-is? Deque?)

;; Splits given integer 'n' into two integer halves whose sum is 'n'.
(define (div2 n)
  (let ((x (round (/ n 2))))
    (values x (- n x))))

;; Constructs a queue with the specified content, balancing first if
;; necessary to meet the invariant.
(define (check lenf f lenr r)
  (cond
   ((> lenf (+ (* (dq-factor) lenr) 1))
    (let*-values (([lenf+ lenr+] (div2 (+ lenf lenr)))
		  ([f+ f-rest] (split-at lenf+ f))
		  ([r+] (append r (reverse f-rest))))
      (Deque lenf+ f+ lenr+ r+)))

   ((> lenr (+ (* (dq-factor) lenf) 1))
    (let*-values (([lenf+ lenr+] (div2 (+ lenf lenr)))
		  ([r+ r-rest] (split-at lenr+ r))
		  ([f+] (append f (reverse r-rest))))
      (Deque lenf+ f+ lenr+ r+)))

   (else
    (Deque lenf f lenr r))))

;; Whether empty.
(define* (dq-empty? q)
  (and (= (Deque-lenf q) 0) (= (Deque-lenr q) 0)))

;; Push element to front.
(define* (dq-push-f q e)
  (check (+ (Deque-lenf q) 1) (cons e (Deque-f q))
	 (Deque-lenr q) (Deque-r q)))

;; Push element to rear.
(define* (dq-push-r q e)
  (check (Deque-lenf q) (Deque-f q) 
	 (+ (Deque-lenr q) 1) (cons e (Deque-r q))))

;; Returns the first element from the front, or failure-result if
;; none.
(define* (dq-peek-f q (failure-result #f))
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0))
      failure-result)

     ;; Note that the invariant guarantees that both 'f' and 'r' are
     ;; non-empty if there are at least two elements in the queue. It
     ;; would perhaps be clearer to write (last r) instead of (first r)
     ;; here, but it's the same either way since we know the length of
     ;; 'r' must be exactly 1 here.
     ((= lenf 0) 
      (first (Deque-r q)))

     (else 
      (first (Deque-f q))))))

;; Returns the first element from the rear, or nil if none.
(define* (dq-peek-r q (failure-result #f))
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0))
      failure-result)

     ((= lenr 0) 
      (first (Deque-f q)))

     (else 
      (first (Deque-r q))))))

;; Drops the first element from the front. Does nothing if there isn't
;; one.
(define* (dq-tail-f q)
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) q)
     ((= lenr 0) dq-empty)
     (else (check (- lenf 1) (rest (Deque-f q)) lenr (Deque-r q))))))

;; Drops the first element from the rear. Does nothing if there isn't
;; one.
(define* (dq-tail-r q)
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) q)
     ((= lenf 0) dq-empty)
     (else (check lenf (Deque-f q) (- lenr 1) (rest (Deque-r q)))))))

;; Return first element from the front, plus all but the first element
;; from the front. Same semantics as for (values (dq-peek-f q)
;; (dq-tail-f q)), but more efficient.
(define* (dq-pop-f q (failure-result #f))
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) 
      (values failure-result q))
     ((= lenf 0) 
      (values (first (Deque-r q)) dq-empty))
     (else 
      (let ((f (Deque-f q)))
	(values (first f)
		(check (- lenf 1) (rest f) lenr (Deque-r q))))))))

;; Appends another deque 'qa' to the front.
(define* (dq-prepend q qa)
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q))
        (qa-lenf (Deque-lenf qa))
        (qa-lenr (Deque-lenr qa)))
    (cond
     ((and (= lenf 0) (= lenr 0)) qa)
     ((and (= qa-lenf 0) (= qa-lenr 0)) q)
     (else 
      (check (+ qa-lenf qa-lenr lenf)
	     (append (Deque-f qa) (reverse (Deque-r qa)) (Deque-f q))
	     lenr (Deque-r q))))))

;; Appends another deque 'qa' to the rear.
(define* (dq-append q qa)
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q))
        (qa-lenf (Deque-lenf qa))
        (qa-lenr (Deque-lenr qa)))
    (cond
     ((and (= lenf 0) (= lenr 0)) qa)
     ((and (= qa-lenf 0) (= qa-lenr 0)) q)
     (else 
      (check lenf (Deque-f q)
	     (+ lenr qa-lenf qa-lenr)
	     (append (Deque-r qa) (reverse (Deque-f qa)) (Deque-r q)))))))
