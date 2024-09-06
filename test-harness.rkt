#lang racket

(provide test)

(define (test message actual expected)
    (if (equal? actual expected) 
        (success message)
        (failure message actual expected)))


(define (failure message actual expected)
    (display "\e[31mTEST: ")
    (display message) 
    (display " ❌")
    (newline)
    (display "\e[31mEXPECT: ")
    (display expected)
    (newline)
    (display "ACTUAL: ")
    (display actual)
    (display "\e[0m")
    (newline)
    (newline))

(define (success message)
 (display "\e[32mTEST: ")
  (display message)
  (display " --> PASSED ✅\e[0m")
  (newline)
  (newline)
)
