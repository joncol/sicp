(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (square x)
  (* x x))

(define (identity x)
  x)

(define (log2 x)
  (/ (log x) (log 2)))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))
