#lang scheme
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union-iter sets result)
  (cond ((null? sets) result)
        ((element-of-set? (car sets) result)
         (union-iter (cdr sets) result))
        (else (union-iter (cdr sets) (append result (list (car sets)))))))

(define (union-set set1 set2)
  (union-iter (append set1 set2) (list )))

(define set1 (list 1 3 4))
(define set2 (list 2 4 5))

(union-set set1 set2)






