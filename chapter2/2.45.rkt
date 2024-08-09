#lang sicp

(#%require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split direction1 direction2) painter (- n 1)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split direction1 direction2) painter (- n 1))))
                (direction1 painter (direction2 smaller smaller))))))