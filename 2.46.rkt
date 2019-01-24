#lang scheme

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vec)
  (car vec))
(define (ycor-vect vec)
  (cdr vec))

(define x (make-vect 1 2))

;add
(define (add-vect vec1 vec2)
  (cons (+ (xcor-vect vec1) (xcor-vect vec2)) (+ (ycor-vect vec1) (ycor-vect vec2))))

(add-vect x x)

;sub
(define (sub-vect vec1 vec2)
  (cons (- (xcor-vect vec1) (xcor-vect vec2)) (- (ycor-vect vec1) (ycor-vect vec2))))

(sub-vect x x)

;scale
(define (scale-vect s vec)
  (cons (* s (xcor-vect vec)) (* s (ycor-vect vec))))

(scale-vect 2.453 x)



