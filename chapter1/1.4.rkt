#lang racket

(require "../test-harness.rkt")

(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

(test "Given a positive b value" (a-plus-abs-b 1 2) 3)
(test "Given a negative b value" (a-plus-abs-b 2 -2) 4)

; The way this works is that the if statement will execute first then the results of that is in the position of an operator. Whatever operator is returned will be applied to the operands on the right.