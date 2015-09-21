(load "line-segments-2-2.scm")

;;; first representation (width . height)

(define (make-rect width height)
  (cons width height))

(define (rect-width rect)
  (car rect))

(define (rect-height rect)
  (cdr rect))


;;; second representation (top-left . bottom-right)

(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

(define (rect-top-left rect)
  (car rect))

(define (rect-bottom-right rect)
  (cdr rect))

(define (rect-width rect)
  (- (x-point (rect-bottom-right rect))
     (x-point (rect-top-left rect))))

(define (rect-height rect)
  (- (y-point (rect-bottom-right rect))
     (y-point (rect-top-left rect))))


;;; operations

(define (rect-perimeter rect)
  (+ (* 2 (rect-width rect))
     (* 2 (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))
