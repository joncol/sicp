(load "../chapter1/common.scm")

;;; exercise 2.45

(define (split proc smaller-proc)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (dec n))))
          (proc painter (smaller-proc smaller smaller)))))
  rec)
