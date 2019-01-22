#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((or (and (< d 0) (> n 0)) (and (< d 0) (< n 0)))
       (cons (/ (- n) g) (/ (- d) g)))
          ((= n 0) (cons 0 d))
          (else (cons (/ n g) (/ d g))))))
     
(define n-one-half (make-rat -1 2))
(print-rat n-one-half)



