(define tolerance 0.001)

(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display "trying: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (define (solution-found)
        (display "solution found")
        (newline)
        next)

      (if (close-enough? guess next)
          (solution-found)
          (try next))))
  (try first-guess))

(define (fun x)
  (/ (log 1000) (log x)))

;;; exercise 1.36: the solution to x^x = 1000
(define answer
  (fixed-point-print fun 2.0))

(define (avg-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;;; 7 steps with damping vs. 23 without damping
(define answer-avg-damp
  (fixed-point-print (avg-damp fun) 2.0))
