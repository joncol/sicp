(load "newtons-method.scm")

;;; exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 2 10 -20) 1)
