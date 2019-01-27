#lang scheme
(define (=zero? x) (null? x))
(define (add x y) (+ x y))
(define (mul x y) (* x y))

;Dense Polynomials
(define (adjoin-term order-diff term term-list)
  ;Adjoin now is done for 1 higher order term than the term-list
  (cond ((=zero? term) term-list)
        ((= order-diff 1) (cons term term-list))
        (else (adjoin-term (- order-diff 1) term (cons 0 term-list)))
        ))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) 
  (null? term-list))

;constructors and selectors
(define (make-term-list term-list)
  (term-list))
(define (order term-list) (- (length term-list) 1))

;
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) 
               (t2 (first-term L2)))
           (cond ((> (order L1) (order L2))
                  (adjoin-term
                    (- (order L1) (order L2)) L1 
                   (add-terms (rest-terms L1) 
                              L2)))
                 ((< (order L1) (order L2))
                  (adjoin-term
                   (- (order L2) (order L1)) L2 
                   (add-terms 
                    L1 
                    (rest-terms L2))))
                 (else
                  (adjoin-term
                    (add t1 
                         t2)
                   (add-terms 
                    (rest-terms L1)
                    (rest-terms L2)))))))))

;Multiplication
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms 
       (mul-term-by-all-terms 
        (order L1) (first-term L1) L2)
       (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms order-t1 t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term (- (+ order-t1 (order L)) (order L))
          (mul t1 (first-term L))
         (mul-term-by-all-terms 
          t1 
          (rest-terms L))))))

;Apply that for dense polynomial