; Test Harness
(define (test procedure expected)
  (display "EXPECT: ")
  (display expected)
  (newline)
  (display "ACTUAL: ")
  (display procedure)
  (newline)
  (newline))
  
(define (count-leaves tree)
  (if (null? tree)
      0
      (if (pair? tree)
          (+ (count-leaves (car tree)) (count-leaves (cdr tree)))
          (+ 1))))

(test (count-leaves (list )) 0)
(test (count-leaves (list 1)) 1)
(test (count-leaves (list 1 2)) 2)
(test (count-leaves (list 1 (list 2 3))) 3)
(test (count-leaves (list 1 (list 2 3 (list 4) 5) 6)) 6)