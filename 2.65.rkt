#lang scheme
; Tree making

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define x (list->tree (list 1 3 5 7 9 11))) ; 
(define y (list->tree (list 2 4 5 6 8 11 12)))
; List making
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;union-set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (union-iter set1 set2 result)
  (cond ((null? set1) result)
        ((element-of-set? (car set1) set2)
         (union-iter (cdr set1) set2 result))
        (else (union-iter (cdr set1) set2 (append result (list (car set1)))))))

(define (union-set set1 set2)
  (union-iter set1 set2 set2))

(define (element-of-tree? x tree)
  (cond ((null? tree) false)
        ((= x (car tree)) true)
        ((< x (car tree)) (element-of-tree? x (left-branch tree)))
        (else (element-of-tree? x (right-branch tree)))))

(define (union-tree-iter list-tree1 tree2 result-list)
  (cond ((null? list-tree1) result-list)
        ((element-of-tree? (car list-tree1) tree2)
         (union-tree-iter (cdr list-tree1) tree2 result-list))
        (else (union-tree-iter (cdr list-tree1) tree2 (append result-list (list (car list-tree1)))))))
          
(define (union-tree tree1 tree2)
  (union-tree-iter (tree->list-1 tree1) tree2 (tree->list-1 tree2)))

x
y
(display "Union-result: ")
(union-tree x y)

;intersection-set
(define (inter-tree-iter list-tree1 tree2 result-list)
  (cond ((null? list-tree1) result-list)
        ((not (element-of-tree? (car list-tree1) tree2))
         (inter-tree-iter (cdr list-tree1) tree2 result-list))
        (else (inter-tree-iter (cdr list-tree1) tree2 (append result-list (list (car list-tree1)))))))
          
(define (intersection-tree tree1 tree2)
  (inter-tree-iter (tree->list-1 tree1) tree2 (list )))

(display "Intersection-result: ")
(intersection-tree x y)




