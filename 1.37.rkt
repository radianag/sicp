#lang racket

(define (cont-frac n d k)
  (if (< k 2)
      (/ n d)
      (/ n (+ d (cont-frac n d (- k 1))))))

(/ 1 (cont-frac 1 1 100))


(define (cont-frac-iter n d k counter result)
  (if (> counter k)
      result
      (cont-frac-iter n d k (+ 1 counter) (/ n (+ d result)))
      )
  )

(/ 1 (cont-frac-iter 1 1 100 1 0))
