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

;make frame and selectors
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define frame1 (make-frame x y z))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;make segment
(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment v)
  (car v))
(define (end-segment v)
  (cadr v))

(define seg-a (make-segment x y))

;add
(define (add-vect vec1 vec2)
  (cons (+ (xcor-vect vec1) (xcor-vect vec2)) (+ (ycor-vect vec1) (ycor-vect vec2))))
;sub
(define (sub-vect vec1 vec2)
  (cons (- (xcor-vect vec1) (xcor-vect vec2)) (- (ycor-vect vec1) (ycor-vect vec2))))
;scale
(define (scale-vect s vec)
  (cons (* s (xcor-vect vec)) (* s (ycor-vect vec))))


;frame-coord-map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

;1) Draw outline
(define outline-segment-list
  (let ((origin (make-vector 0 0))
        (edge1 (make-vector 0 1))
        (edge2 (make-vector 1 0))
        (corner(make-vector 1 1)))
  (list (make-segment origin edge1)
        (make-segment edge1 corner)
        (make-segment corner edge2)
        (make-segment edge2 origin))))

(define draw-outline
  (segments->painter outline-segment-list))

;2) Draw x
(define x-segment-list
  (let ((origin (make-vector 0 0))
        (edge1 (make-vector 0 1))
        (edge2 (make-vector 1 0))
        (corner(make-vector 1 1)))
  (list (make-segment origin corner)
        (make-segment edge2 edge1))))

(define draw-x
  (segments->painter x-segment-list))

;3) Draw diamond
(define diamond-segment-list
  (let ((x1 (make-vector 0 0.5))
        (x2 (make-vector 0.5 1))
        (x3 (make-vector 1 0.5))
        (x4 (make-vector 0.5 0)))
  (list (make-segment x1 x2)
        (make-segment x2 x3)
        (make-segment x3 x4)
        (make-segment x4 x1))))

(define draw-diamond
  (segments->painter diamond-segment-list))

;4) Draw wave? wtf I need a curve function for that to get good segments

