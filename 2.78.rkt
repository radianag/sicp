#lang scheme

(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number)
      (contents)
      (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        ((symbol? datum) 'scheme-symbol)
        (else error "Bad tagged datum: type-tag")))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        ((symbol? datum) datum)
        (else error "Bad tagged datum: 
              CONTENTS" datum)))

  