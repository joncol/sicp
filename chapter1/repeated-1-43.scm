(load "common.scm")
(load "compose-1-42.scm")

;;; exercise 1.43
(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (dec n)))))
