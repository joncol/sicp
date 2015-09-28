;;; exercise 2.17: get the last element of a list
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))
