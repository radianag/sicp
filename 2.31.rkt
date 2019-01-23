#lang racket
(define (square x) (* x x))

(define (tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree factor)
             (factor sub-tree)))
       tree))

(define (square-tree items)
  (tree-map items square))
  
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))