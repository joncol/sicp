(load "accumulate.scm")

;;; exercise 2.33: common list operations

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x len) (+ len 1)) 0 sequence))