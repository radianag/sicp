#lang scheme

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

;append ( 3-1-5) [cons 7 (9-1)]
;append ( [append ( 1 cons 3 5)] [ cons 7 (9 11)] )
; (1 3 5 7 9 11)

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

; (Copy (3-1-5) ((cons 7 (copy (9-11))))
; (Copy (Copy 1 (cons 3 5)) (cons 7 (9 11))))
; ((1 (3 5)) (7 (9 11)))

;They will make same list

;2) The append will take more steps as it is an iterative procedure rather than cons

