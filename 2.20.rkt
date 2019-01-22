#lang racket

(define (same-parity x . z)
  (if (= 0 (remainder x 2))
      (get-parity (append (list x) z) 2 (list ) (length(append (list x) z)))
      (get-parity (append (list x) z) 1 (list ) (length(append (list x) z)))))

(define (get-parity item x result n)
  (if (< n 1)
      result
      (if (= x 2)
          (if (= (remainder (car item) 2) 0)
              (get-parity (cdr item) x (append result (list (car item))) (- n 1))
              (get-parity (cdr item) x result (- n 1)))
          (if (= (remainder (car item) 2) 1)
              (get-parity (cdr item) x (append result (list (car item))) (- n 1))
              (get-parity (cdr item) x result (- n 1))))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 7 8 10)  