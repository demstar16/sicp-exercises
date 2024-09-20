#lang racket

(require "../test-harness.rkt")

(define a 3) ; 3
(define b (+ a 1)) ; 4

(test "Addition" (+ a b (* a b)) 19)
(test "Equality" (= a b) #f)
(test "If" (if (and (> b a) (< b (* a b))) ; (4 > 3) & (4 < 12)
    b
    a) 4)
(test "Cond" (cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25)) 16)
(test "Addition and If" (+ 2 (if (> b a) b a)) 6)

(test "Multiply, Cond and Addition" (* (cond ((> a b) a) ; 3 > 4
        ((< a b) b) ; 3 < 4
        (else -1)) 
    (+ a 1))
16)

