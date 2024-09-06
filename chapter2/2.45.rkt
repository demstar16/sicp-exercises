#lang sicp

(#%require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split direction1 direction2)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split direction1 direction2) painter (- n 1))))
                (direction1 painter (direction2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 2))
(paint (up-split einstein 2))

(define (corner-split painter n)
   (if (= n 0)
      painter
       (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
         (let ((top-left (beside up up))
               (bottom-right (below right right))
               (corner (corner-split painter (- n 1))))
           (beside (below painter top-left)
                   (below bottom-right corner))))))

(define (square-limit painter n)
   (let ((quarter (corner-split painter n)))
     (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 2))