(load "transform-painter.scm")
(load "flip-horiz-2-50.scm")
(load "vector-2-46.scm")

;;; exercise 2.51

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         split-point))
          (paint-up (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))
