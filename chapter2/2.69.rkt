#lang racket

(require "../test-harness.rkt")
(require "../utils.rkt")
(require racket/trace)

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (make-code-tree left right)
    (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
    (adjoin-set x (cdr set))))))
    
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) ; symbol
                            (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))

; SOLUTION CODE
(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (let ((first (car leaves)) (second (cadr leaves)) (rest (cddr leaves)))
        (let ((current-node (make-code-tree first second)))
          (if (null? rest)
              current-node
              (successive-merge (adjoin-set current-node rest)))))))

(test "An empty leaf set returns an empty set" (successive-merge '(())) '())
(test "A leaf set with 1 item returns that one item" (successive-merge (make-leaf-set (list '(A 2)))) '(leaf A 2))
(test "A leaf set with multiple items returns the merge" (successive-merge (make-leaf-set (list '(A 2) '(B 4) '(C 4) '(D 5)))) '(((leaf A 2) (leaf C 4) (A C) 6) ((leaf B 4) (leaf D 5) (B D) 9) (A C B D) 15))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

// TODO: ask to walkthrough how this generates with Rami
(define the-tree '((A 7) (B 9) (C 11) (D 3) (E 52)))
(test "THE TEST" (generate-huffman-tree the-tree) '((leaf E 52) ((leaf C 11) ((leaf B 9) ((leaf A 7) (leaf D 3) (A D) 10) (B A D) 19) (C B A D) 30) (E C B A D) 82))