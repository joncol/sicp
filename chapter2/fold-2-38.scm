;;; exercise 2.38

(load "accumulate.scm")
(load "../chapter1/common.scm")

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))      ;; => ((3/1)/2)/1 = 3/2

(fold-left / 1 (list 1 2 3))       ;; => ((1/1)/2)/3 = 1/6

(fold-right list nil (list 1 2 3)) ;; => (1 (2 (3 nil)))

(fold-left list nil (list 1 2 3))  ;; => (((nil 1) 2) 3)

;;; fold-left is equal to fold-right if the operation is commutative and
;;; associative
