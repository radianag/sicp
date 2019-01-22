#lang racket
(define (square x) (* x x))
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (close-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (average x y) 
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))

; Answer
(define (golden x)
  (fixed-point 
   (lambda (y) (+ 1 (/ 1 y)))
   1.0))

(golden 5)

