#lang racket
;Headers
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))
(define (itself x) x)
(define (square x) (* x x))
(define (add-int x) (+ x 1))
(define (add a b) (+ a b))
(define (minus a b) (- a b))
(define (multiply a b) (* a b))
(define (divide a b) (/ a b))

;prime?
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

;filtered acc
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (not (filter a)) (filtered-accumulate filter combiner null-value term (next a) next b)
          (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
          )))

;1) Answer
(define (sum-squares-prime a b)
  (filtered-accumulate prime? add 0 square a add-int b))
(sum-squares-prime 1 3)

;2) Answer
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd1? i n)
  (= 1 (gcd i n)))

(define (filtered-accumulate2 filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a b) (filtered-accumulate2 filter combiner null-value term (next a) next b)
          (combiner (term a) (filtered-accumulate2 filter combiner null-value term (next a) next b))
          )))

(define (product-rel-prime a b)
  (filtered-accumulate2 gcd1? multiply 1 itself a add-int b))

(product-rel-prime 1 4)




