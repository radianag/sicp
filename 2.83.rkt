#lang scheme
(define (raise x)
  (apply-generic 'raise x))

(define (raise-int x)
  (make-rational x 1))

(define (raise-rational x)
  (make-from-real-imag x 0))

(put 'raise 'int raise-int)
(put 'raise 'rational raise-rational)


