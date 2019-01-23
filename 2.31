#lang racket
(define (square x) (* x x))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (factor sub-tree)))
       tree))

(define (square-tree items)
  (scale-tree items square))
  
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;still need to implement without map