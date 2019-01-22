#lang scheme
;Headers
(define nil '())
(define (square x) (* x x)) 

;1
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
;2
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list2 items)
  (map square items))

;Test
(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))