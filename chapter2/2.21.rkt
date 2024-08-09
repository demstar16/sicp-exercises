; Test Harness
(define (test procedure expected)
  (display "EXPECT: ")
  (display expected)
  (newline)
  (display "ACTUAL: ")
  (display procedure)
  (newline)
  (newline))

; Part 1
(define (square x) (* x x))

(define (square-list x)
  (if (null? x)
      '()
      (cons (square (car x)) (square-list (cdr x))))
)

(test (square-list (list )) '())
(test (square-list (list 1)) '(1))
(test (square-list (list 1 2 3)) '(1 4 9))

; Part 2
(define (square x) (* x x))

(define (square-list2 x)
  (map square x)
)

(test (square-list2 (list )) '())
(test (square-list2 (list 1)) '(1))
(test (square-list2 (list 1 2 3)) '(1 4 9))