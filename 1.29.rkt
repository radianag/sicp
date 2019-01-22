#lang racket
(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (even? n)
  (= (remainder n 2) 0))
(define (add-kh a k h) (+ a (* k h)))
(define (integral-new f a b n)
  (* (/ (* (- b a)) n 3) (simpson f a b (/ (* (- b a)) n) n 0))
  )
(define (simpson f a b h n counter)
  (sum-sim f a add-kh h b counter n))

(define (sum-sim term a next h b counter n)
  (cond ((> counter n) 0)
        ((or (= counter 0) (= counter n)) (+ (term (+ a (* counter h)))
         (sum-sim term a next h b (+ 1 counter) n)))
        ((even? counter) (+ (* 2 (term (+ a (* counter h))))
         (sum-sim term a next h b (+ 1 counter) n)))
        (else (+ (* 4 (term (+ a (* counter h))))
         (sum-sim term a next h b (+ 1 counter) n)))
         ))

(integral cube 0 1 0.01)

(integral-new cube 0 1 100)