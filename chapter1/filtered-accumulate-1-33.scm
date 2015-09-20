(load "common.scm")

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
  (if (= n 2)
      3
      (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (< n 2)
      #f
      (= n (smallest-divisor n))))

;;; exercise 1.33
;;; recursive process (right fold)
(define (filtered-accumulate combiner null-value term a next b pred)
  (if (> a b)
      null-value
      (if (pred a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value
                              term (next a) next b pred))
          (filtered-accumulate combiner null-value
                               term (next a) next b pred))))

;;; a)
(define (sum1 a b)
  (filtered-accumulate + 0 square a inc b prime?))

;;; b)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product1 n)
  (define (is-relatively-prime-to-n x)
    (= 1 (gcd x n)))

  (filtered-accumulate * 1 identity 1 inc (dec n) is-relatively-prime-to-n))
