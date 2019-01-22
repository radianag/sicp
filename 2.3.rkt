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

(define line1 (make-segment (make-point 0 0) (make-point 1 0)))
(define line2 (make-segment (make-point 0 0) (make-point 0 1)))

(print-point (start-segment line1))
(print-point (end-segment line2))

;Headers
(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (average x y) 
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
;represent rectangles
;my idea: two lines with a coincident point
(define (make-rectangle p1 p2 p3)
  (cons (make-segment p1 p2) (make-segment p2 p3)))
(define (b-rect rect) (car rect))
(define (h-rect rect) (cdr rect))

(define (line-dist line) (sqrt (+ (square (- (x-point (end-segment line)) (x-point (start-segment line))))
                                   (square (- (y-point (end-segment line)) (y-point (start-segment line))))
                                   )))

(define (perim-rect rectangle) (+ (* 2 (line-dist (b-rect rectangle))) (* 2 (line-dist (h-rect rectangle)))))
(define (area-rect rectangle) (* (line-dist (b-rect rectangle)) (line-dist (h-rect rectangle))))

(define my-rect (make-rectangle (make-point 0 1) (make-point 0 0) (make-point 1 0)))
(perim-rect my-rect)
(area-rect my-rect)

;Different representation of rectangle
; there is no diff representation other than 3 ind points





