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

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
	(accumulate cons seq1 seq2))
(define (length sequence)
	(accumulate (lambda (x n) (+ n 1)) 0 sequence))

(test "map applies given procedure correctly to the sequence given" (map square '(1 2 3)) '(1 4 9))
(test "append correctly appends second given sequence to first given sequence" (append '(1 2) '(1 2)) '(1 2 1 2))
(test "length returns correct length given a sequence" (length '(1 2 3 4)) 4)