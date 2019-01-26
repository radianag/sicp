#lang scheme

;Raise procedure
(define (raise x)
  (apply-generic 'raise x))

(define (raise-int x)
  (make-rational x 1))

(define (raise-rational x)
  (make-from-real-imag x 0))

(put 'raise 'int raise-int)
(put 'raise 'rational raise-rational)


;The tower
(define (tower-numeric-num type)
  (cond ((= type 'scheme-number) 1)
        ((= type 'rational) 2)
        ((= type 'complex) 3)))

(define tower-numeric (list 'scheme-number 'rational 'complex))

;Project procedure for every type
(define (project-complex x)
  (make-rational (real-part x) 1))

(define (project-rational x)
  (numer x))

(put 'project 'rational project-rational)
(put 'project 'complex project-complex)

(define (project x)
  (apply-generic 'project x))

;Drop to lowest and check
(define (drop num)
  (cond ((= (tower-numeric-num num) 1) num)
        ((= (raise (project num)) num ) (drop (project num)))
        (else num)))




                                       
  


