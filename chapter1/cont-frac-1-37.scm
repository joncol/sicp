(load "common.scm")

;;; exercise 1.37: k-term finite continued fractions
(define (cont-frac-rec n d k)
  (define (run i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (run (inc i))))))
  (run 1))

(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (dec i) (/ (n i) (+ (d i) acc)))))
  (iter k 0.0))

;;; Evaluates to 1/phi ~ 0.61803398875.
;;; Correct to 4 decimal places with k = 10.
(define (test-rec k)
  (cont-frac-rec (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 k))

(define (test-iter k)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k))
