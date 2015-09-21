(load "../chapter1/gcd-1-20.scm")
(load "rat.scm")

;;; exercise 2.1: make-rat
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))
