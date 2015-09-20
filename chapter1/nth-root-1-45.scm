(load "common.scm")
(load "fixed-point.scm")
(load "repeated-1-43.scm")

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;;; exercise 1.45
(define (nth-root n x)
  (let ((m (floor (log2 n))))
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (dec n))))
     (repeated average-damp m)
     1.0)))
