#lang racket
;Headers
(define (square x) (* x x))
(define nil (list ))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

; making unique pairs


;(unique-pairs 7)

;Prime Header?
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

;simplifying prime-sum-pairs
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(define (unique-pairs n)
  (accumulate 
 append
 nil
 (map (lambda (i)
        (map (lambda (j) 
               (list i j))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n))))

;(unique-pairs 5)

(define (unique-triplets-for-n n)
  (map (lambda (i) (append (list n) i)) (unique-pairs (- n 1))))

(define (unique-triplets n)
  (flatmap unique-triplets-for-n (enumerate-interval 1 n)))
(define x (unique-triplets 5))

(define (make-triple-sum triple)
  (list (car triple) 
        (cadr triple)
        (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (make-triple-sum-n n)
  (map make-triple-sum (unique-triplets n)))

(make-triple-sum-n 5)



