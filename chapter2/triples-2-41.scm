(load "enumerate.scm")
(load "../chapter1/common.scm")

;;; exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (dec j))))
                      (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(define (unique-triples-with-sum n sum)
  (define (sum-list l)
    (accumulate + 0 l))
  (filter (lambda (l) (= (sum-list l) sum)) (unique-triples n)))
