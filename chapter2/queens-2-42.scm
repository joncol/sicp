(load "enumerate.scm")
(load "flatmap.scm")
(load "../chapter1/common.scm")

;;; exercise 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens) ;; roq = a list of position pairs
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (dec k)))))) ;; ways to position queens in k-1 columns
  (queen-cols board-size))

(define empty-board nil)

;;; The argument positions is a list of position pairs (row col).
;;; Check the first (row col) in the list against previous
;;; (row col)-pairs for conflicts.
(define (safe? positions)
  (define (conflicts-with? new-pos)
    (lambda (pos)
      (let ((r1 (pos-row pos))
            (c1 (pos-col pos))
            (r2 (pos-row new-pos))
            (c2 (pos-col new-pos)))
        (or (= r1 r2)
            (= c1 c2)
            (let ((slope (/ (- r2 r1) (- c2 c1))))
              (or (= slope 1)
                  (= slope -1)))))))

  (let ((new-p (car positions))
        (old-ps (cdr positions)))
    (not (any? (conflicts-with? new-p) old-ps))))

(define (any? predicate sequence)
  (not (null? (filter predicate sequence))))

(define (make-pos row col)
  (list row col))

(define (pos-row pos)
  (car pos))

(define (pos-col pos)
  (cadr pos))

(define (adjoin-position row col positions)
  (cons (make-pos row col) positions))
