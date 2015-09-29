(load "accumulate.scm")
(load "list-manip-2-33.scm")
(load "../chapter1/common.scm")

;;; exercise 2.35: count-leaves with accumulate

(define (count-leaves t)
  (accumulate (lambda (l count) (+ count (length l)))
              0
              (map enumerate-tree (list t))))

(define (count-leaves2 t)
  (accumulate + 0
              (map (lambda (x) 1) (enumerate-tree t))))
