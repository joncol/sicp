(load "../chapter1/common.scm")

;;; exercise 2.5: pairs (of nonnegative integers) as numbers of form 2^a * 3^b
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (get-factorization-exp z 2))

(define (cdr z)
  (get-factorization-exp z 3))

;;; find the largest number a such that n^a is a factor of x
(define (get-factorization-exp x n)
  (define (iter y result)
    (if (= (remainder y n) 0)
        (iter (/ y n) (inc result))
        result))
  (iter x 0))
