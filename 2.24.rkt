#lang racket

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) 
                     (+ 1 count))))
  (length-iter items 0))

; result from interpretter
(define x (list 1 (list 2 (list 3 4))))
x

;do every else on paper
(car x)
(cdr x)
(car (cdr x))

(newline)
(car (cdr x))
(pair? (car (cdr x)))
(null? (car (cdr x)))
(length (car (cdr x)))

(newline)
(cdr (cdr x))
(pair? (cdr (cdr x)))
(null? (cdr (cdr x)))
(length (cdr (cdr x)))

