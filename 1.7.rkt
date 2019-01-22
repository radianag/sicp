#lang racket
(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) 
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))


(sqrt-iter 2 0.00004)
(sqrt-iter 3 0.00004)
; two different answers

;large number
;(sqrt-iter 1.0 162321451005000000000000) ;doesn't compute

(define (good-enough-new? guess guess_min1)
  (< (abs (- guess guess_min1)) 0.001))

(define (sqrt-iter-new guess guess_min1 x)
  (if (good-enough-new? guess guess_min1)
      guess
      (sqrt-iter-new (improve guess x) guess x)))

(sqrt-iter-new 2 1.0 0.0004)
(sqrt-iter-new 3 1.0 0.0004)

;With good-enough-new?, error stays constant with smaller and larger numbers.