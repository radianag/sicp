#lang scheme

;1) Change wave painter to add smile. I don't know how to do that since I didn't make wave yet in
; exercise 2.49


;2) Change corner-spllit to use one up and one beside
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

;3) Assemble corners in different pattern
(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-vert 
                         rotate180
                         identity 
                         flip-horz)))
    (combine4 (corner-split painter n))))

