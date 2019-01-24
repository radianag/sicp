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

(define (sqrt x)
(fixed-point (average-damp (lambda (y) (/ x y)))
1.0))

(sqrt 25)

(define (expt b n)
(if (= n 0)
1
(* b (expt b (- n 1)))))


(define (n-root n x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) (- n 2))
               1.0 ))

(n-root 3 8)


