(load "common.scm")

;;; exercise 1.41: apply function twice
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)
