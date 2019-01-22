#lang racket
(define (cube x) (* x x x))
(define (p x count)
  (display count)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle count)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0) (+ count 1)) (+ count 1))))

(sine 12.15 1)
(display "\n")
(sine 1 1)
(display "\n")
(sine 2 1)
(display "\n")
(sine 3 1)
(display "\n")
(sine 10 1)
(display "\n")
(sine 100 1)
(display "\n")

;1) 6 calls of p
;2) O(log(n))



