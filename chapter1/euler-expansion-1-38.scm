(load "common.scm")
(load "cont-frac-1-37.scm")

(define (d i)
  (if (= (remainder (- i 2.0) 3.0) 0.0)
      (* (/ (inc i) 3.0) 2.0)
      1.0))

;;; exercise 1.38
;;; evaluates to e ~ 2.71828182846
(define (test-e k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0)
                       d
                       k)))
