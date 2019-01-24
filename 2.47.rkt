#lang scheme

(define (make-vect x y)
  (cons x y))
(define (xcor-vect vec)
  (car vec))
(define (ycor-vect vec)
  (cdr vec))

(define x (make-vect 0 0))
(define y (make-vect 1 2))
(define z (make-vect 2 4))

;make frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define frame1 (make-frame x y z))
(define frame2 (make-frame2 x y z))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;test selectors make-frame
(origin-frame frame1)
(edge1-frame frame1)
(edge2-frame frame1)

;for make-frame2 all selectors are the same except edge2 selector
(define (2-edge2-frame frame)
  (cddr frame))

;test selectors make-frame2
(newline)
(origin-frame frame2)
(edge1-frame frame2)
(2-edge2-frame frame2)









