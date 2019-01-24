#lang racket


(define (cont-frac-iter n d k counter result f g)
  (cond ((> counter k) result)
        ((= 1 counter) (cont-frac-iter n d k (+ 1 counter) (/ n (+ (g d) result)) f g))
        (else (cont-frac-iter n d k (+ 1 counter) (/ (f n) (+ (g d) result)) f g))
        )
  )

        
(define (tan-cf x k)
  (cont-frac-iter 1 x k 1 0 (lambda (y) (* y y)) (lambda (z) (- (* z 2) 1))) 
  )

(tan-cf 2.124 100)