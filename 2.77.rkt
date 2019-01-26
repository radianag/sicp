#lang scheme

(define (magnitude z) 
  (apply-generic 'magnitude z))
;Apply generic looks under magnitude and applies any operation present


(put 'magnitude '(rectangular) magnitude)
(put 'magnitude '(polar) magnitude)

;by using the data structure
(define z (complex rectangular 3 4))

(magnitude z)
(apply generic (complex rect 3 4))
;tries to find the 'complex tag which is not found in table

;Install
(put 'magnitude '(complex) magnitude)

; Makes
(apply generic (complex rect 3 4))
(magnitude (rect 3 4))
(apply generic (rect 3 4))
(magnitude (3 4))
5



