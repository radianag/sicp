#lang racket
(define (cons x y) 
  (lambda (m) (m (expt 2 x) (expt 3 y))))

(define (car z) 
  (z (lambda (p q) p)))

(define (cdr z) 
  (z (lambda (p q) q)))

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; EXPAND ADD-1
;(add-1 zero)
;(add-1 (lambda (f) (lambda (x) x)))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

;SIMPLIFY
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

;; Expand two, using the simplified version of one:
;; (add-1 one)
;; (add-1 (lambda (f) (lambda (x) (f x))))
 (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;;
;; Again, here we perform some internal optimizations.
;;
 (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;; (lambda (f) (lambda (x) (f (f x))))

;; And here's the definition of two.  It is a function that takes a
;; function f and returns a function that applies f twice.

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; Now to define add directly.  

(define (add a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))


