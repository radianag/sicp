#lang racket
(define nil (list ))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)(cons (car s) x)) rest)))))

(define x (list 1 2 3 4))
(subsets x)

(cons (car x) (subsets (cdr x)))