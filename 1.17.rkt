#lang racket


(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (display b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 2 11)


