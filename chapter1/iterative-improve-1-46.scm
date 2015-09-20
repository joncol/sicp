(load "common.scm")

;;; exercise 1.46
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve-guess)
         (improve-guess guess)))))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve-guess guess)
    ((average-damp (lambda (y) (/ x y))) guess))
  ((iterative-improve good-enough? improve-guess) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) 0.0001))
  (define improve-guess f)
  ((iterative-improve good-enough? improve-guess) 1.0))j
