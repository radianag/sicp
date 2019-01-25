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

;Decode Tree
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

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


(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(display "sample-tree :")
sample-tree


(define key (lambda (x) (caddr x)))

(define (encode-symbols letter tree result)
  ;(newline)
  ;(display letter)
  ;(display result)
  (cond ((leaf? tree) result)
        ((equal? (car (key tree)) letter)
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

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-decoded-message (decode sample-message sample-tree))
(newline)
(display "sample-decoded-message: ")
sample-decoded-message

(define sample-encoded-message (encodes sample-decoded-message sample-tree))
(newline)
(display "sample-encoded-message: ")
sample-encoded-message





