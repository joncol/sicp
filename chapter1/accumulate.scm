#lang planet neil/sicp

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

;;; exercise 1.32
;;; recursive process (right fold)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;;; iterative process (left fold)
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))
