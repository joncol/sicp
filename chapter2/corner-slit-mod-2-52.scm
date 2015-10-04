(load "painters-2-49.scm")
(load "../chapter1/common.scm")

;;; exercise 2.52

(define (corner-split-mod painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (dec n)))
            (right (right-split painter (dec n))))
        (let ((top-left (beside up x-painter))
              (bottom-right (below right x-painter))
              (corner (corner-split-mod painter (dec n))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit-mod painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
