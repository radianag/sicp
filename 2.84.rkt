#lang scheme
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))\
                (if (= type1 type2)
                    (apply-generic 
                          op a1 a2)
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond ((< (tower-numeric type1) (tower-numeric type2))
                        (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2)))
                        ((t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

(define (tower-numeric type)
  (cond ((= type 'scheme-number) 1)
        ((= type 'rational) 2)
        ((= type 'complex) 3)))

;change tower-numeric if you want to have more types in the data-structure

