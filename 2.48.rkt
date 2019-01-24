#lang scheme

;make vector and selectors
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vec)
  (car vec))
(define (ycor-vect vec)
  (cdr vec))

(define x (make-vect 0 0))
(define y (make-vect 1 2))
(define z (make-vect 2 4))

;make segment
(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment v)
  (car v))
(define (end-segment v)
  (cadr v))

(define seg-a (make-segment x y))
(start-segment seg-a)
(end-segment seg-a)


