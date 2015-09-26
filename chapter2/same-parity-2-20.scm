(load "reverse-2-18.scm")

;;; exercise 2.20
(define (same-parity . nums)
  (define (parity x)
    (remainder x 2))
  (let ((first-parity (parity (car nums))))
    (define (iter nums result)
      (cond ((null? nums)
             result)
            ((= first-parity (parity (car nums)))
             (iter (cdr nums) (cons (car nums) result)))
            (else (iter (cdr nums) result))))

    (reverse  (iter nums '()))))
