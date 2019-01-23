#lang racket
(define nil (list ))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y)) 0 (map (lambda (x) x) (enumerate-tree t))))

(define x (list 1 (list 2 (list 3 4)) 5))

(count-leaves x)

