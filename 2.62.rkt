#lang scheme

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (union-iter set1 set2 result)
  (cond ((null? set1) result)
        ((element-of-set? (car set1) set2)
         (union-iter (cdr set1) set2 result))
        (else (union-iter (cdr set1) set2 (append result (list (car set1)))))))

(define (union-set set1 set2)
  (union-iter set1 set2 set2))

(define set1 (list 1 3 4))
(define set2 (list 2 4 5))

(union-set set1 set2)
