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

;; exercise 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; substitution model illustration of (+ 4 5):
;; (+ 4 5)
;;   (inc (+ 3 5))
;;   (inc (inc (+ 2 5)))
;;   (inc (inc (inc (+ 1 5))))
;;   (inc (inc (inc (inc (+ 0 5)))))
;;   (inc (inc (inc (inc 5))))
;;   (inc (inc (inc 6)))
;;   (inc (inc 7))
;;   (inc 8)
;;   9
;; => recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; substitution model illustration of (+ 4 5):
;; (+ 4 5)
;;   (+ 3 6)
;;   (+ 2 7)
;;   (+ 1 8)
;;   (+ 0 9)
;;   9
;; => iterative process

;; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n)) ;; 2 * n

(define (g n) (A 1 n)) ;; 2 ^ n

;; 2 4 16 65536, ... = 2^1, 2^2, 2^4, 2^16, ... = 2, 2^2, 2^2^2, 2^2^2^2, ...
(define (h n) (A 2 n))

;; exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (g n)
  (define (f-iter a b c count)
    (if (= count 0)
        a
        (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f-iter 0 1 2 n))

;; exercise 1.12 - Pascal's triangle (curried)
(define (pascal i)
  (lambda (j)
    (if (or (= j 0) (= i j))
        1
        (+ ((pascal (- i 1)) (- j 1)) ((pascal (- i 1)) j)))))

(define (upto n)
  (define (aux count)
    (if (= count n)
        (list n)
        (cons count (aux (+ count 1)))))
  (aux 0))

(define (row n) (map (pascal n) (upto n)))

(define (rows n) (for-each (lambda (n) (print (row n))) (upto (- n 1))))

(define (triangle n)
  (for-each (lambda (n)
              (begin (display (map (lambda (x)
                                     (if (even? x)
                                         #\
                                         #\#)) (row n))) (newline)))
            (upto (- n 1))))

;; exercise 1.14
(define (count-change amount)
  (cc amount 5))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

;; drew evaluation graph of (count-change 11) on paper
;; Exponential time, O(n) space ??

;; exercise 1.15
;; 5 times ((1 / 3) ^ 5 * 12.15 = 0.05, (1 / 3) ^ 4 * 12.15 = 0.15)
;; O(log(a)) time and space
