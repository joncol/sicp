(load "common.scm")
(load "repeated-1-43.scm")

;;; exercise 1.44: smoothed function
(define (smooth f)
  (let ((dx 0.001))
    (lambda (x)
      (/ (+ (- (f x) dx) (f x) (+ (f x) dx)) 3.0))))

;;; n-fold smoothed function
(define (n-fold-smooth f n)
  ((repeated smooth n) f))
