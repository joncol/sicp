(load "../chapter1/common.scm")

;;; exercise 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (dec n))))
        (below painter (beside smaller smaller)))))
