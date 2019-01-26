#lang scheme

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

; Need to add methods of rational and others to methods: make-from-mag-ang, make-from-real-imag

;Changes--> Should I just change the constructors?
;NO, you need to change representation of complex in rational numbers, everything in the install-complex-package will work
;need to change the primitive operators in (make-from-mag-ang polar, make-from-real-imag rect), such as: cos, sin, atan, square. 

(define (install-complex-rational-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  
  ;; internal procedures
  (define (cos-rat x)
    ()); dont know how
  (define (sin-rat x)
    ())
  (define (atan-rat x)
    ())
  (define (square-rat x)
    (make-rat (square (numer x)) (square (denom x))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'rational z))
  (put 'square 'rational
       (lambda (z1) 
         (tag (square z1))))
  (put 'cos 'rational
       (lambda (z1) 
         (tag (cos-rat z1))))
  (put 'sin 'rational
       (lambda (z1) 
         (tag (sin-rat z1))))
  (put 'atan 'rational
       (lambda (z1) 
         (tag (atan-rat z1))))
  'done)
