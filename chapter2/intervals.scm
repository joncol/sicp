(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound x))
                               (/ 1.0 (lower-bound y)))))

 ;;; exercise 2.7
(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

;;; exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; exercise 2.10
(define (div-interval2 x y)
  (if (> (* (lower-bound y) (upper-bound y)) 0)
      (div-interval x y)
      (error "Interval y spans 0.")))

;;; exercise 2.11
(define (mul-interval2 x y)
  (define (interval-sign x)
    (cond ((and (>= lo 0) (>= hi 0)) 1)
          ((and (< lo 0) (< hi 0)) -1)
          (else 0)))
  (let ((x-lo (lower-bound x))
        (x-up (upper-bound x))
        (y-lo (lower-bound y))
        (y-up (upper-bound y)))
    (let ((x-sign (interval-sign x))
          (y-sign (interval-sign y)))
      (cond ((> x-sign 0)
             (cond ((> y-sign 0)
                    (make-interval (* x-lo y-lo) (* x-up y-up)))
                   ((< y-sign 0)
                    (make-interval (* x-up y-lo) (* x-lo y-up)))
                   (else (make-interval (* x-up y-lo) (* x-up y-up)))))
            ((< x-sign 0)
             (cond ((> y-sign 0)
                    (make-interval (* x-lo y-up) (* x-up y-lo)))
                   ((< y-sign 0)
                    (make-interval (* x-up y-up) (* x-lo y-lo)))
                   (else (make-interval (* x-lo y-up) (* x-lo y-lo)))))
            (else
             (cond ((> y-sign 0)
                    (make-interval (* x-lo y-up) (* x-up y-up)))
                   ((< y-sign 0)
                    (make-interval (* x-up y-lo) (* x-lo y-lo)))
                   (else
                    (make-interval (min (* x-lo y-up) (* x-up y-lo))
                                   (max (* x-lo y-lo) (* x-up y-up))))))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;; exercise 2.12
(define (make-center-percent c p)
  (let ((w (/ (* c p) 100.0)))
    (make-center-width c w)))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

;;; exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;; no concept of identity: A / A is not 1
