;;; example 2.3.2: symbolic differentiation, data implementation

(load "diff.scm")
(load "diff-data.scm")

(define (contains? sequence item)
  (not (null? (filter (lambda (x) (eq? x item)) sequence))))

;;; sum

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

;;; product

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

;;; exponentiation

(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
        ((= exp 1) base)
        ((not (number? exp)) (error "exponent must be a number" exp))
        (else (list base '** exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base e)
  (car e))
