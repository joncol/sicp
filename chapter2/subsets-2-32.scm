(load "../chapter1/common.scml")

;;; exercise 2.32

(define (subsets s)
  (if (null? s)
      ;; base case
      (list nil)
      ;; The recursive step means we form the list of all subsets of the set
      ;; extended by the element (car s), by first finding all subsets of the
      ;; set of elements without the element (=rest), then prepending the
      ;; element itself to these, and finally taking the union of the subsets
      ;; without the element (rest) and all the subsets formed by prepending
      ;; the element (car s) to the subsets without the element.
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (tail)
                            (cons (car s) tail)) rest)))))
