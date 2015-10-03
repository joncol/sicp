;;; exercise 2.48

(define (make-segment start-vec end-vec)
  (cons start-vec end-vec))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
