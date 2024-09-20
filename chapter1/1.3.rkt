#lang racket

(require "../test-harness.rkt")

(define (sum-square-of-largest-two num1 num2 num3)
    (cond
        [(min num1) (+ (* num2 num2) (* num3 num3))]
        [(min num2) (+ (* num1 num1) (* num3 num3))]
        [(min num3) (+ (* num1 num1) (* num2 num2))]
    ))

(test "Given all different numbers, selects the largest 2 for the operation" (sum-square-of-largest-two 1 2 3) 13)
(test "Given 2 numbers are the same and the 3rd is smaller" (sum-square-of-largest-two 1 2 2) 8)
(test "Given 2 numbers are the same and the 3rd is larger" (sum-square-of-largest-two 2 2 3) 13)
(test "Given 3 numbers are the same" (sum-square-of-largest-two 4 4 4) 32)