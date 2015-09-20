(load "common.scm")
(load "cont-frac-1-37.scm")

;;; exercise 1.39
;;; evaluates tan x, x is in radians
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))

  (define (d i)
    (- (* 2 i) 1))

  (cont-frac-iter n d k))
