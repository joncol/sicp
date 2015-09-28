;;; exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight structure)
  (if (pair? structure)
      (+ (total-weight (left-branch structure))
         (total-weight (right-branch structure)))
      structure))

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (or (not (pair? mobile))
      (let ((l (left-branch mobile))
            (r (right-branch mobile)))
        (and (= (torque l) (torque r))
             (balanced? (branch-structure l))
             (balanced? (branch-structure r))))))

;;; To make these work, we only need to change from cadr to cdr in right-branch
;;; and branch-structure.
(define (make-mobile2 left right)
  (cons left right))

(define (make-branch2 length structure)
  (cons length structure))
