#lang racket

(require sicp-pict)
(require "./paint-utils.rkt")

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
(paint-to-png (right-split einstein 2) "chapter2/2.45a.png")
(paint-to-png (up-split einstein 2) "chapter2/2.45b.png")
