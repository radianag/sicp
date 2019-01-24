#lang scheme
(define nil (list ))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))


;Answer
(define (sum-check list1 list2)
(accumulate + 0 (map (lambda (y) (accumulate + 0 (map (lambda (x) (if (eq? x y) 1 0)) list1))) list2))
)

(define (equal? list1 list2)
  (if (= (sum-check list1 list2) (length list1)) true false))


;Test Answer
(equal? '(this is a list) 
        '(this is a list))

(equal? '(this is a list) 
        '(this (is a) list))


