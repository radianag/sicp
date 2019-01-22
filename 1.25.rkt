#lang racket
(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt b (/ n 2))))
        (else 
         (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod2 base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod2 base (- exp 1) m))
          m))))

; different expod calculates remainder of square then remainder again
; allysia just remainder of all squared number