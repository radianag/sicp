#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (seqs-n select seq)
  (if (null? seq)
      '()
      (cons (select seq)
	    (seqs-n select (cdr seq)))))

(define (car-n seq)
  (seqs-n caar seq))
(define (cdr-n seq)
  (seqs-n cdar seq))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (car-n seqs))
	    (accumulate-n op init (cdr-n seqs)))))

(define matrix-w (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(car-n matrix-w)
