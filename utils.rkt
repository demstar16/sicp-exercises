#lang racket

(provide filter
         reject
         range
         accumulate
         flatmap
         compose
         fold-left
         fold-right)

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
