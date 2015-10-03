(load "vector-2-46.scm")

;;; exercise 2.49

(define outline-painter
  (let ((tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (bl (make-vect 0 0))
        (br (make-vect 1 0)))
    (segments->painter (list (make-segment tl tr)
                             (make-segment tr br)
                             (make-segment br bl)
                             (make-segment bl tl)))))

(define x-painter
  (let ((tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (bl (make-vect 0 0))
        (br (make-vect 1 0)))
    (segments->painter (list (make-segment tl br)

(define diamond-painter
  (let ((ml (make-vect 0.0 0.5))
        (mt (make-vect 0.5 1.0))
        (mr (make-vect 1.0 0.5))
        (mb (make-vect 0.5 0.0)))
    (segments->painter (list (make-segment ml mt)
                             (make-segment mt mr)
                             (make-segment mr mb)
                             (make-segment mb ml)))))
