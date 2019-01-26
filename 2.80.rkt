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
(define (=zero?-scheme x)
  (= x 0))

(define (=zero?-complex x)
  (and (= (real-part x) 0) (= (imag-part x) 0))
  )

(define (=zero?-rational x)
  (and (= (numer x) 0) (= (denom x) 0))
  )
;Install
(put '=zero? 'scheme-number =zero?-scheme)
(put '=zero? 'complex =zero?-complex)
(put '=zero? 'rational =zero?-rational)

;Define generic =zero?
(define (=zero? x)
  (apply-generic '=zero? x))
