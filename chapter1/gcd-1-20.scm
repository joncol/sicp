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
