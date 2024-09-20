#lang racket

(require "../test-harness.rkt")
(require "../utils.rkt")

(define (adjoin-set x set)
    (cond ((null? set) (cons x '()))
            ((= x (car set)) set)
            ((< x (car set)) (cons x set))
            ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(test "Adding to null" (adjoin-set 3 null) '(3))
(test "Adding to an empty list" (adjoin-set 3 '()) '(3))
(test "adding to a list that already has the item we are adding" (adjoin-set 3 '(3 4 5)) '(3 4 5))
(test "Adding to a list where item is smallest in list" (adjoin-set 3 '(4 5 6)) '(3 4 5 6))
(test "Adding to a list where the item is in the middle of the list" (adjoin-set 3 '(1 2 4 5)) '(1 2 3 4 5))
(test "Adding to a list where the item is the biggest in the list" (adjoin-set 3 '(0 1 2)) '(0 1 2 3))