#lang racket
(define (squares x) (* x x)) 
(define (sum-squares x y) (+ (squares x) (squares y) ) )

(define (less-than a b) (if (< a b) a b) )
(define (least x y z)  (less-than (less-than x y) z) ) 

(define (not-least x y z) (cond ((=(least x y z) x) (y z) ) 
				((=(least x y z) y) (x z) )
				( else (x y) ) ) )

(define (largest-two-sum-squares x y z) (sum-squares (not-least x y z) ) )

(largest-two-sum-squares 1 2 3) 