#lang racket

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define line (make-segment (make-point 0 0) (make-point 0 1)))

(print-point (start-segment line))
(print-point (end-segment line))

(define (average x y) (/ (+ x y) 2))
(define (mid-point line)
  (cons (average (x-point (end-segment line)) (x-point (start-segment line)))
        (average (y-point (end-segment line)) (y-point (start-segment line))))
  )

(print-point (mid-point line))




