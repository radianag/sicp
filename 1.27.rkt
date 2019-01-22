#lang racket

(define (square x) (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (newline)
    (display n)
    (newline)
    (display a)
    (display " ")
    (display (expmod a n n))
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fast-prime? 7 1)
(fast-prime? 10 1)
(fast-prime? 561 1)
(fast-prime? 1105 1)
(fast-prime? 1729 1)

;these non prime carmichael numbers really do fool the fermat test with congruency