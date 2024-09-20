#lang racket

(require "../test-harness.rkt")

(define (cadr list)
    (car (cdr list)))

(test "(list 'a 'b 'c) -> (a b c)" (list 'a 'b 'c) '(a b c))
(test "(list (list 'george)) -> ((george))" (list (list 'george)) '((george)))
(test "(cdr '((x1 x2) (y1 y2))) -> (y1 y2)" (cdr '((x1 x2) (y1 y2))) '((y1 y2)))
(test "(cadr '((x1 x2) (y1 y2))) -> x2" (cadr '((x1 x2) (y1 y2))) '(y1 y2)) ; REMEMBER: cdr will always return in a list, it's a pair of what you get and the empty list. So even if it's one item, when using cdr, it will return a list or in this case since it is already a list, a nested list.
(test "(pair? (car '(a short list))) -> #f" (pair? (car '(a short list))) #f)
(test "(memq 'red '((red shoes) (blue socks))) -> #f" (memq 'red '((red shoes) (blue socks))) #f)
(test "(memq 'red '(red shoes blue socks)) -> (red shoes blue socks)" (memq 'red '(red shoes blue socks)) '(red shoes blue socks))

