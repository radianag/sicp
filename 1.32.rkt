#lang racket

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (itself x) x)
(define (add-int x) (+ x 1))
(define (add a b) (+ a b))
(define (minus a b) (- a b))
(define (multiply a b) (* a b))
(define (divide a b) (/ a b))

(define (sum term a next b)
  (accumulate add 0 term a next b))

(define (product term a next b)
  (accumulate multiply 1 term a next b))

;1) Answer
(sum itself 1 add-int 4)
(product itself 1 add-int 4)

;2) Iterative Accumulate
(define (accumulate-iter combiner decombiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        (decombiner result b)
        (iter (next a) (combiner result (term a)))
        ))
  (iter a b))

(define (sum-iter term a next b)
  (accumulate-iter add minus 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter multiply divide 1 term a next b))

(sum-iter itself 1 add-int 4)
(product-iter itself 1 add-int 4)


