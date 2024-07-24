; Test Harness
(define (test procedure expected)
  (display "EXPECT: ")
  (display expected)
  (newline)
  (display "ACTUAL: ")
  (display procedure)
  (newline)
  (newline))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse x)
  (if (null? x)
      '()
      (append (reverse (cdr x)) (list (car x))))
  )

(test (reverse (list )) '())
(test (reverse (list 1)) '(1))
(test (reverse (list 1 2 3)) '(3 2 1))