#lang racket

(require "../test-harness.rkt" "../utils.rkt")


(define is-square-horizontally-threatened-by-queen? eq?)

(display "---------------------TESTING HORIZONTAL THREATS-------------------")(newline)
(test "Given the queens are on the same row, returns true" (is-square-horizontally-threatened-by-queen? 1 1) #t)
(test "Given the queens are not on the same row, returns false" (is-square-horizontally-threatened-by-queen? 1 2) #f)

(define (is-square-BL-TR-threatened-by-queen? queen-row target-row col-offset)
    (eq? (- queen-row col-offset) target-row)
)

(display "-------------------TESTING: DIAGONAL BL-TR THREATS------------------")(newline)
(test "Given the queens are in adjacent columns and the queens are on the same row, returns false" (is-square-BL-TR-threatened-by-queen? 2 2 1) #f)
(test "Given the target and a queen are in adjacent columns, and the queen is one row above the target, returns false" (is-square-BL-TR-threatened-by-queen? 2 1 0) #f)
(test "Given the target and a queen are in adjacent columns, and the queen is one row below the target, returns true" (is-square-BL-TR-threatened-by-queen? 2 1 1) #t)
(test "Given the target and queen are 2 columns apart, and the queen is two rows below the square, returns true" (is-square-BL-TR-threatened-by-queen? 3 1 2) #t)

(define (is-square-BR-TL-threatened-by-queen? queen-row target-row col-offset)
    (eq? (+ queen-row col-offset) target-row)
)

(display "-------------------TESTING: DIAGONAL BR-TL THREATS------------------")(newline)
; |---|---|
; | Q | T |
; |---|---|
(test "Given the queens are in adjacent columns and the queens are on the same row, returns false" (is-square-BR-TL-threatened-by-queen? 1 1 1) #f)

; |---|---|
; | Q |   |
; |---|---|
; |   | T |
; |---|---|
(test "Given the target and a queen are in adjacent columns, and the queen is one row above the target, returns true" (is-square-BR-TL-threatened-by-queen? 1 2 1) #t)

; |---|---|
; |   | T |
; |---|---|
; | Q |   |
; |---|---|
(test "Given the target and a queen are in adjacent columns, and the queen is one row below the target, returns false" (is-square-BR-TL-threatened-by-queen? 2 1 1) #f)

; |---|---|---|
; |   |   | T |
; |---|---|---|
; |   |   |   |
; |---|---|---|
; | Q |   |   |
; |---|---|---|
(test "Given the target and queen are 2 columns apart, and the queen is two rows below the square, returns false" (is-square-BR-TL-threatened-by-queen? 3 1 2) #f)

(define (is-square-threatened-by-queen? queen-row target-row col-offset)
   (or (is-square-BL-TR-threatened-by-queen? queen-row target-row col-offset)
   (is-square-BR-TL-threatened-by-queen? queen-row target-row col-offset)
   (is-square-horizontally-threatened-by-queen? queen-row target-row))
)

(display "-------------------TESTING: IS TARGET CELL THREATENED------------------")(newline)
; |---|---|
; |   | T |
; |---|---|
; | Q |   |
; |---|---|
(test "BL-TR ✅ BR-TL ❌ HORIZONTAL ❌, returns true" (is-square-threatened-by-queen? 2 1 1) #t)

; |---|---|---|
; |   |   | T |
; |---|---|---|
; | Q |   |   |
; |---|---|---|
(test "BL-TR ❌ BR-TL ❌ HORIZONTAL ❌, returns false" (is-square-threatened-by-queen? 2 1 2) #f)

; |---|---|
; | Q |   |
; |---|---|
; |   | T |
; |---|---|
(test "BL-TR ❌ BR-TL ✅ HORIZONTAL ❌, returns true" (is-square-threatened-by-queen? 1 2 1) #t)

; |---|---|
; | Q | T |
; |---|---|
(test "BL-TR ❌ BR-TL ❌ HORIZONTAL ✅, returns true" (is-square-threatened-by-queen? 1 1 1) #t)

; |---|---|---|---|
; |   | Q |   |   |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; |   |   | Q | T |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; | Q |   |   |   |
; |---|---|---|---|
(test "BL-TR ✅ BR-TL ✅ HORIZONTAL ✅, returns true" 
(and (is-square-threatened-by-queen? 1 3 2)
(is-square-threatened-by-queen? 3 3 1)
(is-square-threatened-by-queen? 6 3 3)) #t)


; |---|---|---|---|
; |   |   | Q |   |
; |---|---|---|---|
; |   |   |   | T |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; |   | Q |   |   |
; |---|---|---|---|
(test "BL-TR ✅ BR-TL ✅ HORIZONTAL ❌, returns true" 
(and (is-square-threatened-by-queen? 1 2 1)
(is-square-threatened-by-queen? 4 2 2)
) #t)


; |---|---|---|
; | Q |   |   |
; |---|---|---|
; |   |   |   |
; |---|---|---|
; |   | Q | T |
; |---|---|---|
(test "BL-TR ❌ BR-TL ✅ HORIZONTAL ✅, returns true" 
(and (is-square-threatened-by-queen? 1 3 2)
(is-square-threatened-by-queen? 3 3 1)
) #t)

; |---|---|---|
; |   | Q | T |
; |---|---|---|
; |   |   |   |
; |---|---|---|
; | Q |   |   |
; |---|---|---|
(test "BL-TR ✅ BR-TL ❌ HORIZONTAL ✅, returns true" 
(and (is-square-threatened-by-queen? 3 1 2)
(is-square-threatened-by-queen? 1 1 1)
) #t)

;-----------------------------------------------------------------------

(define (caar list) (car (car list)))

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define (to-position row col) (cons row col))

; I think the issue is around here
(define (adjoin-position row positions)
    (append positions (list row)))

(test "adjoin-position: given an empty list" (adjoin-position 1 '()) '(1))

(define empty-board null)

(define (get-position-row position)
   (car position))

(define (get-position-col position)
   (car (cdr position)))

(define (enumerate-interval low high)
(if (> low high)
null
(cons low (enumerate-interval (+ low 1) high))))


(define (safe? column positions)
    (define kth-queen-row (list-ref positions (- column 1)))
    (define (column-safe? current-column rest-of-positions)
        (if (= current-column column)
            #t
            (if (not (is-square-threatened-by-queen? (car rest-of-positions) kth-queen-row (- column current-column)))
                (column-safe? (+ current-column 1) (cdr rest-of-positions))
                #f
            )
        )
    )
    (column-safe? 1 positions)
)

; |---|---|---|---|
; | Q |   |   |   |
; |---|---|---|---|
; |   |   |   | T |
; |---|---|---|---|
; |   | Q |   |   |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; |   |   | Q |   |
; |---|---|---|---|
 (test "returns true given non threatening positions" (safe? 4 '(1 3 5 2)) #t)
; |---|---|---|---|
; | Q |   |   |   |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; |   |   |   |   |
; |---|---|---|---|
; |   | Q |   |   |
; |---|---|---|---|
; |   |   | T |   |
; |---|---|---|---|
 (test "returns false given threatening positions" (safe? 3 '(1 4 5)) #f)

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                    (flatmap
                        (lambda (rest-of-queens)
                            (map (lambda (new-row)
                                (adjoin-position new-row rest-of-queens))
                            (enumerate-interval 1 board-size)))
                        (queen-cols (- k 1))))))
    (if (= board-size 0)
        '()
        (queen-cols board-size)))

(test "Board size of 0, returns zero solutions" (queens 0) '())
(test "Board size of 1, returns one solution" (queens 1) '((1)))
(test "Board size of 2, returns zero solutions" (queens 2) '())
(test "Board size of 3, returns zero solutions" (queens 3) '())
; |---|---|---|---|
; |   | Q |   |   |
; |---|---|---|---|
; |   |   |   | T |
; |---|---|---|---|
; | Q |   |   |   |
; |---|---|---|---|
; |   |   | Q |   |
; |---|---|---|---|
(test "Board size of 4, returns two solutions" (queens 4) '((2 4 1 3) (3 1 4 2)))