; Test Harness
(define (test message procedure expected)
  (display "TEST: ")
  (display message)
  (newline)
  (display "EXPECT: ")
  (display expected)
  (newline)
  (display "ACTUAL: ")
  (display procedure)
  (newline)
  (newline))

; Dot Product
(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(test "result from 2 vectors of length 1"
      (dot-product (list 1) (list 1))
      1)
(test "result from 2 vectors of length 3"
      (dot-product (list 1 2 3) (list 1 2 3))
                   14)

; Matrix * Vector
(define (matrix-*-vector m v)
   (map (lambda (row) (dot-product row v)) m))

(test "| 1 | * (1)" (matrix-*-vector (list (list 1)) (list 1)) '(1))
(test "| 1 1 | * (1,2)" (matrix-*-vector (list (list 1 1)) (list 1 2)) '(3))
(test "| 1 |
      | 1 | * (2)" (matrix-*-vector (list (list 1) (list 1)) (list 2)) '(2 2))
(test "| 1 2 3 |
      | 1 2 3 | * (1,2,3)" (matrix-*-vector (list (list 1 2 3) (list 1 2 3)) (list 1 2 3))
                           '(14 14))

; Transpose
(define (transpose mat)
   (accumulate-n cons null mat))

(test "| 1 |" (transpose '((1))) '((1)))
(test "| 1 |
      | 2 |" (transpose '((1)(2))) '((1 2)))
(test "| 1 2 |" (transpose '((1 2))) '((1)(2)))
(test "| 1 2 |
      | 3 4 |" (transpose '((1 2)(3 4))) '((1 3)(2 4)))

; Matrix * Matrix
(define (matrix-*-matrix m n)
   (let ((cols (transpose n)))
     (map (lambda (row) (matrix-*-vector cols row)) m)))

(test "| 3 | * | 2 |" (matrix-*-matrix '((3)) '((2))) '((6)))

(test "| 2 4 | * | 1 |
                | 3 |" (matrix-*-matrix '((2 4)) '((1)(3))) '((14)))

(test "| 1 2 | * | 2 3 |
                | 1 4 |" (matrix-*-matrix '((1 2)) '((2 3)(1 4))) '((4 11)))

(test "| 1 2 | * | 2 3 |
      | 3 2 |   | 1 4 |" (matrix-*-matrix '((1 2)(3 2)) '((2 3)(1 4))) '((4 11)(8 17)))