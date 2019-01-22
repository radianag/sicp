#lang racket
(define nil '())

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
  (if (not (pair? items))
      0
      (length-iter items 0)))

(define (deep-reverse items)
  deep-reverse-append items (list ) (- (length items) 1))

(define (deep-reverse-append items result n)
  (if (< n 0) result
      (if (> (length (list-ref items n)) 0)
          (if (null? result)
              (deep-reverse-append items (list (deep-reverse (list-ref items n))) (- n 1))
              (deep-reverse-append items (list (result deep-reverse (list-ref items n))) (- n 1)))
          (deep-reverse-append items (append result (list (list-ref items n))) (- n 1)))
  )
)

(define x 
  (list (list 1 2) (list 3 4) 2))

(define y
  (list 1 2))

(deep-reverse y)















