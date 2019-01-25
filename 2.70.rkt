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

;selector leaf
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
        
;Huffman Tree
(define weight-merge-key (lambda (x) (cadddr x)))

(define (successive-merge leaf-pairs)
  (succ-merge-iter leaf-pairs (list )))

(define (succ-merge-iter leaf-pairs tree)
  (cond ((null? leaf-pairs) tree)
        ((null? tree)
         (succ-merge-iter (cddr leaf-pairs) (make-code-tree (car leaf-pairs) (cadr leaf-pairs))))
        ((< (weight-merge-key tree) (weight-leaf (car leaf-pairs)))
         (succ-merge-iter (cdr leaf-pairs) (make-code-tree (tree) (car leaf-pairs))))
        (else (succ-merge-iter (cdr leaf-pairs) (make-code-tree (car leaf-pairs) tree)))
        ))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

;Encode definitions
(define symbol-key (lambda (x) (caddr x)))

(define (encode-symbols letter tree result)
  (cond ((leaf? tree) result)
        ((equal? (car (symbol-key tree)) letter)
         (encode-symbols letter (left-branch tree) (append result (list '0))))
        (else (encode-symbols letter (right-branch tree) (append result (list '1))))))
  
(define (encodes message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbols (car message) 
                      tree
                      (list ))
       (encodes (cdr message) tree))))


;Make the Tree
(define leaf-pairs (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16) (list 'SHA 3) (list 'YIP 9) (list 'WAH 1)))
(define sample-tree (generate-huffman-tree leaf-pairs))

sample-tree

;Messages to Encode
(list 'GET 'A 'JOB)
(list 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA)

(list 'GET 'A 'JOB)
(list 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA)

(list 'WAH 'YIP 'YIP 'YIP 'YIP)
(list 'YIP 'YIP 'YIP 'YIP 'YIP)

(list 'SHA 'BOOM)


;Encode the stuff

(encodes (list 'GET 'A 'JOB) sample-tree)
(encodes (list 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA) sample-tree)
(encodes (list 'GET 'A 'JOB) sample-tree)
(encodes (list 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA) sample-tree)
(encodes (list 'WAH 'YIP 'YIP 'YIP 'YIP) sample-tree)
(encodes (list 'YIP 'YIP 'YIP 'YIP 'YIP) sample-tree)
(encodes (list 'SHA 'BOOM) sample-tree)

;95 bits to encode song with variable length
;with fixed length: song will be 3 x (34 words) = 102 bits