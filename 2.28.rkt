#lang racket
(define x 
  (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (fringe items)
  (fringe-append items '() (- (length items) 1)))

(define (fringe-append items result n)
  (if (< n 0)
      (