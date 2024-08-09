#lang racket

(define a 3) ; 3
(define b (+ a 1)) ; 4

(+ a b (* a b)) ; (+ 3 4 (* 3 4)) = 19
(= a b) ; #f

(if (and (> b a) (< b (* a b))) ; (4 > 3) & (4 < 12)
    b
    a) 
; 4

(cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25))
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond ((> a b) a) ; 3 > 4
        ((< a b) b) ; 3 < 4
        (else -1)) 
    (+ a 1))
; 4 * (3 + 1) = 16

