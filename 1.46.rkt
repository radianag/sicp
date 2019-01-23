#lang racket

(define (square x) (* x x))
(define (inc x)
  (+ x 1))
(define (average x y) (/ (+ x y) 2))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (< n 2)
      f
      (repeated (compose f f) (- n 1))))

;((repeated square 2) 5)

(define (average-damp f)
(lambda (x) (average x (f x))))

(define (close-enough? v1 v2)
(< (abs (- v1 v2))
tolerance))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
(try first-guess))

(define (sqrt x)
(fixed-point (average-damp (lambda (y) (/ x y)))
1.0))

(define (iterative-improve check improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (check guess next)
          next
          (try next))))
  (lambda (guess) (try guess)))

(define (sqrt2 x)
  ((iterative-improve close-enough? (average-damp (lambda (y) (/ x y)))) 1.0))

(sqrt 16)
(sqrt2 16)