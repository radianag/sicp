#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) 
                     (+ 1 count))))
  (length-iter items 0))

(define (reverse items)
  (reverse-append items (list ) (- (length items) 1)))

(define (reverse-append items result n)
  (if (< n 0)
      result
      (reverse-append items (append result (list (list-ref items n))) (- n 1))))

(reverse (list 1 4 9 16 25))

