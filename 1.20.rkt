#lang racket

(define (gcd a b)
  (display a)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)