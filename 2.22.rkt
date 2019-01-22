#lang racket
;Headers
(define nil '())
(define (square x) (* x x)) 

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))
;The cons square is done from 1st and put in front (car). The 2nd is put in car of the 1st.

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))

(square-list2 (list 1 2 3 4))
;Answer will be be nil and never updated