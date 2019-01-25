#lang scheme
;Tree construct and selector
(define (entry tree) (car tree))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (make-tree entry left right)
  (list entry left right))

;constructor leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

;selector leaft
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;constructor tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

;selector tree
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;Adjoin
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

;Make-leaf-set
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))
        
;Answer Definition
(define key (lambda (x) (cadddr x)))

(define (successive-merge leaf-pairs)
  (succ-merge-iter leaf-pairs (list )))

(define (succ-merge-iter leaf-pairs tree)
  (cond ((null? leaf-pairs) tree)
        ((null? tree)
         (succ-merge-iter (cddr leaf-pairs) (make-code-tree (car leaf-pairs) (cadr leaf-pairs))))
        ((< (key tree) (weight-leaf (car leaf-pairs)))
         (succ-merge-iter (cdr leaf-pairs) (make-code-tree (tree) (car leaf-pairs))))
        (else (succ-merge-iter (cdr leaf-pairs) (make-code-tree (car leaf-pairs) tree)))
        ))

; Test
(define leaf-pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

;Answer
(generate-huffman-tree leaf-pairs)






