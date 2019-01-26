#lang scheme
;Header apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

;Define each type method
(define (equ?-scheme x y)
  (= x y))

(define (equ?-complex x y)
  (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))
  )

(define (equ?-rational x y)
  (and (= (numer x) (numer y)) (= (denom x) (denom y)))
  )
;Install
(put 'equ? 'scheme-number equ?-scheme)
(put 'equ? 'complex equ?-complex)
(put 'equ? 'rational equ?-rational)

;Define generic equ?
(define (equ? x y)
  (apply-generic 'equ? x y))
