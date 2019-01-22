#lang racket
(define (square x) (* x x))
(define (fast-expt b n)
  (display b)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt b (/ n 2))))
        (else 
         (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


(fast-expt 2 4)

(define (expt b n) 
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (display product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(expt 2 4)

