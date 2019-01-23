#lang racket

(define (square x) (* x x))
(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (< n 2)
      f
      (repeated (compose f f) (- n 1))))

((repeated square 2) 5)

(define (smooth f)
  (define dx 0.00001)
  (lambda(x) (+ (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  (lambda (x)(repeated f n) x ))

((n-fold-smooth square 5) 4)