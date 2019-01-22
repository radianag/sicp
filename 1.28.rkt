#lang racket
(define (square x) (* x x))
(define (fast-prime-iter n counter)
    (cond ((= counter 1) #t) ; There is no need to check 1
          ((miller-rabin-test n counter)
           (fast-prime-iter n (- counter 1)))
          (else 
            (newline)
            (display counter)
            #f)))

(define (fast-prime? n)
  (fast-prime-iter n (- n 2)))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
             (remainder (sqrmod-with-check (expmod base (/ exp 2) m) m)
                        m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))

(define (miller-rabin-test n a)  
  (= (expmod a (- n 1) n) 1))

(define (sqrmod-with-check val n)
  (let ((sqrmod (remainder (square val) n)))
    (cond ((or (= val (- n 1)) (= val 1)) sqrmod)
          ((= sqrmod 1) 0)
          (else sqrmod))))

(fast-prime? 7)
(fast-prime? 10)
(fast-prime? 561)
(fast-prime? 1105)
(fast-prime? 1729)