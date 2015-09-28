;;; exercise 2.18
(define (reverse items)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (car l) acc))))
  (iter items ()))
