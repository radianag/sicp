#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


(define (itself x) x)
(define (add-int x) (+ x 1))

;Factorial using Product
(define (factorial a)
(product itself 1 add-int a))
(factorial 5)

;Pi Approximation


;Product Iter
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        (/ result b)
        (iter (next a) (* result (term a)))
        ))
  (iter a b))

(product-iter itself 1 add-int 4)
