#lang scheme

(list 2 3 2 1 3 2 2)

;Same
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;Dont need check if its in there or not. O(n) to O(1)
(define (adjoin-set x set)
  (cons x set))

;Intersection is the same.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

;Yes, the adjoin set deletes the element-of-set check so it's faster. This is for maybe
; appending data together.

