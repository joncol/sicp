(define (identity x)
  x)

(define (inc x)
  (+ 1 x))

;;; exercise 1.31

(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

(define (product-iter factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (factor a)))))
  (iter a 1))

;;; formula by John Wallis
(define (quarter-of-pi n)
  (define (next x)
    (inc (inc x)))
  (define (sqr x)
    (* x x))
  (if (odd? n)
      (/ (* 2 (product-iter sqr 4 next (inc n)))
         (* (+ n 2) (product-iter sqr 3 next n)))
      (/ (* 2 (product-iter sqr 4 next n) (+ n 2))
         (product-iter sqr 3 next (inc n)))))
