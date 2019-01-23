#lang scheme

(define a (list 1 3 (list 5 7) 9))
(define b (list(list 7)))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (check-equal? num x)
  (if (= num x) true false))

a
(check-equal? (car (cdr (car (cdr (cdr a))))) 7)
b
(check-equal? (car (car b)) 7)
c
(check-equal? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))) 7)

(newline)
(define x (car (cdr (cdr a))))
x
(pair? x)
(null? x)