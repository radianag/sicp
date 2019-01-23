#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))


(define (make-center-width c w)
(make-interval (- c w) (+ c w)))
(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* (/ p 100) c)))

(define a (make-center-percent 2 0.5))

(define b (make-center-percent 10 1))

(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
(p2 (* (lower-bound x) (upper-bound y)))
(p3 (* (upper-bound x) (lower-bound y)))
(p4 (* (upper-bound x) (upper-bound y))))
(make-interval (min p1 p2 p3 p4)
(max p1 p2 p3 p4))))

(define (mul-percent-aprox pa pb)
  (+ pa pb))
(display "Percentage approximation    :")
(mul-percent-aprox 0.5 1)
(newline)

(define c (mul-interval a b))
(define (get-percent x)
  (let ((c (/ (+ (upper-bound x) (lower-bound x)) 2)))
  (* (/ (- (upper-bound x) c) c) 100)))

(display " Actual Percentage    :")
(get-percent c)




