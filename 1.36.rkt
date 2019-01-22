#lang scheme

(define (square x) (* x x))
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (newline)
    (display v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (close-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (average x y) 
  (/ (+ x y) 2))

; Answer
(define (x-sqr-x x)
  (fixed-point 
   (lambda (y) (/ (log 1000) (log y)))
   x))

(x-sqr-x 1.5)

(define (x-sqr-x-damped x)
  (fixed-point 
   (lambda (y) (average (/ (log 1000) (log y)) y))
   x))

;Answer 2
(newline)
(newline)
(x-sqr-x-damped 1.5)

;average damped is faster


