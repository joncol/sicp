(load "union-set-2-59.scm")

;;; exercise 2.60

;; element-of-set? is the same as before.

;; adjoin-set is simpler now: no need to check any previous elements
(define adjoin-set cons)

;; union-set is also simpler and faster now
(define union-set append)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
