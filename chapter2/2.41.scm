#lang sicp
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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(test "ordered-triples: Given 0" (ordered-triples 0) '())
(test "ordered-triples: Given a number that is incapable of producing an orderd triple" (ordered-triples 2) '())
(test "ordered-triples: Given lowest possible number that will produce an ordered triple" (ordered-triples 3) '((3 2 1)))
(test "ordered-triples: Given an a number that will produce multiple ordered triples" (ordered-triples 5) '((3 2 1)(4 2 1)(4 3 1)(4 3 2)(5 2 1)(5 3 1)(5 3 2)(5 4 1)(5 4 2)(5 4 3)))

 (define (triple-sum? triple s)
   (= s (accumulate + 0 triple)))

(test "triple-sum?: Doesn't sum to s" (triple-sum? '(1 1 1) 4) #f)
(test "triple-sum?: Does sum to s" (triple-sum? '(2 1 1) 4) #t)

(define (make-triple-sum triple)
  (append triple (list (accumulate + 0 triple))))

(test "make-triple-sum: correctly appends the sum" (make-triple-sum '(1 2 3)) '(1 2 3 6))

(define (ordered-triple-sum n s)
  (define (nested-triple-sum? triple)
   (= s (accumulate + 0 triple)))
  (map make-triple-sum
       (filter nested-triple-sum?
               (ordered-triples n))))

(test "ordered-triple-sum: Given 0 0" (ordered-triple-sum 0 0) '())
(test "ordered-triple-sum: Given a 's' value that can't be met by the ordered triples" (ordered-triple-sum 4 30) '())
(test "ordered-triple-sum: Given a 's' value that will be met by some combination" (ordered-triple-sum 8 6) '(3 2 1 6))
(test "ordered-triple-sum: Given a 'n' and 's' value that will produce a list with multiple values" (ordered-triple-sum 10 10) '((5 3 2 10) (5 4 1 10) (6 3 1 10) (7 2 1 10))) 
