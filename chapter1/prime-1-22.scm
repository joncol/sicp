(load "../chapter1/smallest-divisor-1-21.scm")

;; exercise 1.22

(define (prime? n)
  (if (< n 2)
      #f
      (= n (smallest-divisor n))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (fast-prime? n 10)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  ;; aux assumes a and b are odd
  (define (aux a b)
    (timed-prime-test a)
    (when (<= a b)
      (search-for-primes (+ a 2) b)))

  (aux (if (odd? a) a (+ a 1))
       (if (odd? b) b (- b 3))))
