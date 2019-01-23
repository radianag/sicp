#lang scheme
(define x 
  (list (list 1 2) (list 3 4)))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (fringe x)
  (cond ((null? x) (list ))
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                 (fringe (cdr x))))))

(fringe x)
(fringe (list x x))


