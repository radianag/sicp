#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


 (define (width x)
   (/ (- (upper-bound x) (lower-bound x)) 2))

(define a (make-interval 1 2))
(define b (make-interval 3 4))

(define (add-width x y)
  (+ (width x) (width y)))

(define (sub-width x y)
  (+ (width x) (width y)))

(define (mul-width x y)
  (* (width x) (width y)))

;Compare width and add-width
(width (add-interval b a))
(add-width b a) ;produces same result

(width (sub-interval b a))
(sub-width b a) ;produces same result

(width (mul-interval b a))
(mul-width b a); not same







