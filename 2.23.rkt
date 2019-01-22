#lang racket
;Headers
(define nil '())


(define (for-each f item)
  (f (car item))
  (if (null? (cdr item))
      true
  (for-each f (cdr item))))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

