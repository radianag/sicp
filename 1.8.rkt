#lang racket
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (cube-average x y z) 
  (/ (+ x y z) 3))
(define (improve-cube guess x)
  (cube-average guess guess (/ x (square guess))))
  
(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-iter (improve-cube guess x) x)))

(cube-iter 5.2 64)