(load "fixed-point.scm")

;;; exercise 1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
