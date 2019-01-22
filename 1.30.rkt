#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (itself x) x)
(define (add-int x) (+ x 1))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        (- result b)
        (iter (next a) (+ result (term a)))
        ))
  (iter a b))

(sum-iter itself 1 add-int 4)


