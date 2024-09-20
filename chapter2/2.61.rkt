#lang racket

(require "../test-harness.rkt")
(require "../utils.rkt")

(define (adjoin-set x set)
    (cond [(eq? set '()) (cons x set)]
            [(< x (car set)) (cons x set)]
            [(> x (car set)) (cons (car set) (adjoin-set x (cdr set)))]
            [else set])
)

(test "Adjoining a number with empty set" (adjoin-set 3 '()) '(3))
(test "Adjoining an element to a non-empty set that does not contain it" (adjoin-set 3 '(2)) '(2 3))
(test "Adjoining an element that will be the smallest in the set" (adjoin-set 3 '(4 5)) '(3 4 5))
(test "Adjoining a unique element that is within the bounds of the set" (adjoin-set 3 '(2 4)) '(2 3 4))
(test "Adjoining a non-unique element" (adjoin-set 3 '(1 2 3 4 5)) '(1 2 3 4 5))