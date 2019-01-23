#lang racket
(define nil '( ))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;1
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

;1-test
(define matrix-w (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define vector-v (list 1 0 0 0))

(matrix-*-vector matrix-w vector-v)

;2
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))


(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose matrix-w)

;3
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
      (map (lambda (x)
	     (accumulate-n + 0 
			   (transpose (map (lambda (y)
					     (map * x y))
					   cols))))
	   m)))

(matrix-*-matrix matrix-w (transpose matrix-w))
