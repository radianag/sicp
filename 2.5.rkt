#lang racket

(define (expt b n)
  (if (= n 0) 
      1 
      (* b (expt b (- n 1)))))

(define (cons x y) 
  (lambda (m) (m (expt 2 x) (expt 3 y))))

(define (car z) 
  (z (lambda (p q) p)))

(define (cdr z) 
  (z (lambda (p q) q)))


(define a (cons 1 2))
(car a)
(cdr a)

