#lang racket

(require "../test-harness.rkt")
(require "../utils.rkt")

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
      (cons (entry tree)
        (tree->list-1
          (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
      (cons (entry tree)
        (copy-to-list
          (right-branch tree)
            result-list)))))
  (copy-to-list tree '()))

(define tree1 '(5 (3 (2 () ()) (9 () ())) (7 (1 () ()) (10 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

; The procedure do produce the same results. The only difference is how they are doing it. 
; They both do an in-order traversal
;
; The first procedure has a time complexity of O(nlogn) due to the use of append.
; Append's time complexity is dependent upon the length of the first list passed into it.
; Which is the left sub-tree, approximately half of the tree.
;
; The second procedure just calls cons at every entry node in the tree.
; Which is just O(n).
