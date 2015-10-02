(load "accumulate.scm")
(load "../chapter1/common.scm")

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
