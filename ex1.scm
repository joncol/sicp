;; exercise 1.1

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; exercise 1.2

(/ (+ 4 (+ 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; exercise 1.3

(define (sqr x)
  (* x x))

(define (sum-of-squares x y)
  (+ (sqr x) (sqr y)))

(define (sum-of-squares-of-two-largest x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= y x) (<= y z)) (sum-of-squares x z))
        ((and (<= z x) (<= z y)) (sum-of-squares x y))))

;; exercise 1.4
;; a-plus-abs-b computes a + b if b > 0, and a - b otherwise
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))
;; applicative order => infinite recursion
;; normal order      => 0


;; exercise 1.6
;; the process would never bottom-out, since the second parameter to new-if
;; would always be evaluated (due to applicative order evaluation)

;; exercise 1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; This algorithm is not very exact for small numbers, due to limited precision.
;; For large numbers the algorithm never finishes, probably since subtracting
;; a very large number from another very large number leads to rounding errors.

(define (sqrt-iter2 last-guess guess x)
  (if (good-enough-ratio? last-guess guess)
      guess
      (sqrt-iter2 guess
                  (improve guess x)
                  x)))

(define (good-enough-ratio? last-guess guess)
  (< (abs (/ (- guess last-guess) last-guess)) 0.001))

(define (sqrt2 x)
  (sqrt-iter2 2.0 1.0 x))

;; sqrt2 works much better

;; exercise 1.8 - Newton's method for cube roots

(define (cube-root x)
  (define (cube-root-iter last-guess guess x)
    (if (good-enough-ratio? last-guess guess)
        guess
        (cube-root-iter guess
                        (improve guess x)
                        x)))

  (define (improve guess x)
    (/ (+ (/ x (sqr guess)) (* 2 guess)) 3))

  (cube-root-iter 2.0 1.0 x))
