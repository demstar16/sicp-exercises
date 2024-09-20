#lang racket

(require "../test-harness.rkt")

; 5 + 4 + (2 - (3 - (6 + 4/5)))
;------------------------------
;       3(6 - 2)(2 - 7)

(test "Test expression validity" (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))) '-37/150)