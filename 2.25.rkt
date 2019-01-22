#lang racket

(define a '(1 3 '(5 7) 9))
(define b '('(7)))
(define c '(1 '(2 '(3 '(4 '(5 '(6 7)))))))

a
b
c

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) (if (= 7 x) x 1))
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves c)
