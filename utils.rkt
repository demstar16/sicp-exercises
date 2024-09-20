#lang racket

(provide filter
         reject
         range
         accumulate
         flatmap
         compose
         fold-left
         fold-right
         make-product
         make-sum
         =number?
         multiplicand
         multiplier
         augend
         addend
         product?
         sum?
         variable?
         same-variable?)

(define (filter predicate sequence)
  (cond
    [(null? sequence) null]
    [(predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence)))]
    [else (filter predicate (cdr sequence))]))

(define (reject predicate sequence)
  (filter (lambda (x) (not (predicate x))) sequence))

(define (range start end)
  (if (> start end) null (cons start (range (+ start 1) end))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (fold-right op initial sequence)
  (if (null? sequence) initial (op (car sequence) (fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))