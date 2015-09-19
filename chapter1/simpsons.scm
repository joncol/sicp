#lang planet neil/sicp

(define (cube x)
  (* x x x))

;;; exercise 1.29 (Simpson's Rule)
(define (inc x)
  (+ x 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define (si-term x)
    (cond ((= x 0) (f a))
          ((= x n) (f b))
          ((odd? x) (* 4 (f (+ a (* x (/ (- b a) n))))))
          ((even? x) (* 2 (f (+ a (* x (/ (- b a) n))))))))
  (* (/ (/ (- b a) n) 3) (sum si-term 0 inc n)))
