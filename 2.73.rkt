#lang scheme

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;1) The 'derive is put into table. now it is possible to call with (get 'deriv (operator exp) operands)
; Meaning it will search the correct function in 'derive that will operate for the given operator of exp
; then do this on the operands exp.
; number? and variable? are not put in 'deriv because they work on the primitives of the scheme language

;2) deriv of sums and products and auxiliary code to install?
(define (install-deriv-sum-product)
  ;; internal procedures
  (define (deriv-sum exp var) (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
  ;; interface to the rest of the system
  (put '(deriv) '+  deriv-sum)
  (put '(deriv) '* deriv-product)
  'done)

;3)
(define (install-deriv-exponent)
  ;; internal procedures
  (define (deriv-exponentiation exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                   (deriv (base exp) var)))
  ;; interface to the rest of the system
  (put  'deriv '** deriv-exponentiation)
  'done)

;4)change the put like this:
  (put '+ '(deriv) deriv-sum)
  (put '* '(deriv) deriv-product)
