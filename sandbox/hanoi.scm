;;; Towers of Hanoi

(define (move n from to spare)
  (if (= n 1)
      (show-move from to)
      (begin
        (move (- n 1) from spare to)
        (move 1 from to spare)
        (move (- n 1) spare to from)))
  )

(define (show-move from to)
  (begin (display from) (display " -> ") (display to) (newline)))
