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
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

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

;; exercise 1.16
;; calculates b^n in O(log n) time
(define (fast-expt-iter b n)
  (define (fast-expt-iter-aux b n a)
    (if (= n 0)
        a
        (if (even? n)
            (fast-expt-iter-aux (square b) (/ n 2) a)
            (fast-expt-iter-aux b (- n 1) (* a b)))))
  (fast-expt-iter-aux b n 1))

;; exercise 1.17
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-mult x y)
  (if (= 1 x)
      y
      (if (even? x)
          (fast-mult (halve x) (double y))
          (+ y (fast-mult (- x 1) y)))))

;; exercise 1.18
(define (fast-mult-iter x y)
  (define (fast-mult-iter-aux x y a)
    (if (= x 1)
        (+ a y)
        (if (even? x)
            (fast-mult-iter-aux (halve x) (double y) a)
            (fast-mult-iter-aux (- x 1) y (+ a y)))))
  (fast-mult-iter-aux x y 0))

;; exercise 1.19
;; T(p, q): a <- bq + aq + ap
;;          b <- bp + aq
;; Apply T(p, q) twice. Yields transformation T(p', q') of same form.
;; Derivation on paper yields: p' = p^2 + q^2 and q' = 2pq + q^2.
;; O(log n) time and O(1) space. Cool!
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; How many remainder operations in normal order evaluation of (gcd 206 40)?
;; [gcd 206 40]:
;;   (gcd 40 (remainder 206 40)) ... (0)
;;   [gcd 40 6]:
;;     (if (= b 0) (1)
;;     (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;;     [gcd 6 4]:
;;       (if (remainder 40 (remainder 206 40)) = 0 ... (2)
;;       (gcd (remainder 40 (remainder 206 40))
;;            (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;;       [gcd 4 2]:
;;         (if (= (remainder (remainder 206 40)
;;                           (remainder 40 (remainder 206 40))))) ... (4)
;;         (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;              (remainder (remainder 40 (remainder 206 40))
;;                         (remainder (remainder 206 40)
;;                                    (remainder 40 (remainder 206 40))))
;;       [gcd 2 0]:
;;         (if (= b 0) ... (7)
;;         a ... (4)
;; Total: 18
;;
;; In applicative order evaluation of (gcd 206 40)?
;; [gcd 206 40]:
;;   (gcd 40 (remainder 206 40)) ... (1)
;;   [gcd 40 6]:
;;     (gcd 6 (remainder 40 6)) ... (1)
;;     [gcd 6 4]:
;;       (gcd 4 (remainder 6 4)) ... (1)
;;       [gcd 4 2]:
;;         (gcd 2 (remainder 4 2)) ... (1)
;;         [gcd 2 0]:
;;           done
;; Total: 4

;; exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)   ; 199
(smallest-divisor 1999)  ; 1999
(smallest-divisor 19999) ; 7

;; exercise 1.22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  ;; aux assumes a and b are odd
  (define (aux a b)
    (timed-prime-test a)
    (if (<= a b)
        (search-for-primes (+ a 2) b)))

  (aux (if (odd? a) a (+ a 1))
       (if (odd? b) b (- b 3))))

;; at values of around 100000000000, it starts to be apparent that the running
;; time of the algorithm indeed is O(n^0.5)

;; exercise 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; The speedup ratio is about 1.65. Probably because of highly optimized
;; addition operator compared to function call to next, with contains branching.
;; I.e. the condition checking in next takes considerable time in comparison to
;; the primality test.

;; exercise 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; O(log n) means we have to (roughly) double the number of digits to double
;; the time. Observations support this.

;; exercise 1.25
(define (expmod base exp m)
  (remainder (fast-expt-iter base exp) m))
;; using this expmod is much slower, probably since repeatedly calculating the
;; remainder of very large numbers divided by eachother is slower than when
;; having smaller numbers
