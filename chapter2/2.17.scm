; Test Harness
(define (test procedure expected)
  (display "EXPECT: ")
  (display expected)
  (newline)
  (display "ACTUAL: ")
  (display procedure)
  (newline)
  (newline))

(define (last-pair x)
(if (= (length (cdr x)) 0)
    ‘(car x)
    (last-pair (cdr x))))

(test (last-pair (list 1 2 3 4 34 5 643)) '(643))
(test (last-pair (list 1 6)) '(6))
(test (last-pair (list 1 2 3 4 5 643 2 3 4)) '(4))
(test (last-pair (list 1)) '(1))