#lang scheme
;From problem
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))


;my answer
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))


;test
(define a (make-branch 1 1.5))
(define b (make-branch 2 (make-mobile a a)))
(define c (make-mobile a b))

(define (count-leaves2 x)
  (cond ((null? x) 0)
        ((not (pair? x)) 0)
        ((and (not (pair? (car x))) (null? (cdr x))) (car x))
        (else (+ (count-leaves2 (car x))
                 (count-leaves2 (cdr x))))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;Total weight
(define (total-weight mobile)
  (count-leaves2 mobile))

c
(total-weight c)

