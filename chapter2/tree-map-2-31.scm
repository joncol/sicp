(load "../chapter1/common.scm")

;;; exercise 2.31

(define (tree-map fun tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fun sub-tree)
             (fun sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
