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

; Part A
; A binary mobile consists of BRANCHES, each BRANCH consists of a length and either a weight (number) or a binary mobile which will then consist of 2 more BRANCHES.

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (cadr list) (car (cdr list)))
(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

(test "Basic left-branch" (left-branch (make-mobile 1 2)) 1)
(test "One level of nesting left-branch" (left-branch (make-mobile (make-mobile 3 4) 2)) '(3 4))
(test "Multiple levels of nesting in left-branch" (left-branch (make-mobile (make-mobile 1 (make-mobile 2 3)) 5)) '(1 (2 3)))

(test "Basic right-branch" (right-branch (make-mobile 1 2)) 2)
(test "One level of nesting right-branch" (right-branch (make-mobile 2 (make-mobile 3 4))) '(3 4))
(test "Multiple levels of nesting in right-branch" (right-branch (make-mobile 5 (make-mobile 1 (make-mobile 2 3)))) '(1 (2 3)))

(test "branch-length returns the correct length given a branch" (branch-length '(4 (make-mobile 2 1))) 4)

(test "branch-structure returns the correct structure given a branch" (branch-structure '(3 2)) 2)
(test "branch-structure returns the correct structure given a branch" (branch-structure '(3 (make-mobile (make-mobile 1 2) 3))) '(make-mobile (make-mobile 1 2) 3))

; Part B
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
        struct
        (total-weight struct))))

(test "total-weight returns the correct weight of a mobile given only 2 branches" (total-weight (make-mobile (make-branch 1 1) (make-branch 1 2))) 3) 
(test "total-weight returns the correct weight of a mobile given nested branches" (total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 1 4) (make-branch 1 3))) (make-branch 1 8))) 15) 

; Part C
(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced-mobile? mobile)
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (balanced-mobile? (branch-structure branch))
        #t))
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

; |----|
; |    |
;(1,1)(1,1)
(define balancedMobile0 (make-mobile (make-branch 1 1) (make-branch 1 1)))
(test "given a mobile where each branch ends in a weight,
and the torque of each branch is the same, it returns true" (balanced-mobile? balancedMobile0) #t)

;    |-----------|
;    |           |
;  (3, |----|) (9,5)
;      |    |
;     (2,5)(1,10)
(test "given a mobile where one branch ends in a balanced mobile,
 and the other branch ends in a weight, and the torque of each branch
is the same, it returns true" (balanced-mobile? balancedMobile1) #t)

;    |-----------|
;    |           |
;  (3, |----|) (3,10)
;      |    |
;     (2,1)(1,10)
(test "given a mobile where one branch ends in a unbalanced mobile,
 and the other branch ends in a weight, and the torque of each branch at the root
is the same, it returns false" (balanced-mobile? unbalancedMobile1) #f)

;    |-----------|
;    |           |
;  (3, |----|) (3,4)
;      |    |
;     (2,1)(1,10)
(test "given a mobile where one branch ends in a unbalanced mobile,
and the other branch ends in a weight,
and the torque of each branch at the root
is NOT the same, it returns false" (balanced-mobile? unbalancedMobile2) #f)

;    |-------------------|
;    |                   |
;  (2, |----|)        (2,|------|)
;      |    |            |      |  
;     (2,5)(1,10)       (2,5)  (1,10)
(define balancedMobile2 (make-mobile (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 1 10)))
                                     (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 1 10)))))
(test "given a mobile where each branch ends in a balanced mobile,
and the torque at the root level is the same, it returns true" (balanced-mobile? balancedMobile2) #t)

;    |-------------------|
;    |                   |
;  (2, |----|)        (2,|------|)
;      |    |            |      |  
;     (2,10)(1,5)       (2,5)  (1,10)
(define unbalancedMobile3  (make-mobile (make-branch 2 (make-mobile (make-branch 2 10) (make-branch 1 5)))
                                        (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 1 10)))))
(test "given a mobile where one branch ends in an unbalanced mobile,
and the other branch ends in a balanced mobile,
and the torque at the root level is equal,
it returns false." (balanced-mobile? unbalancedMobile3) #f)

; |
; (1 2)
(test "multiplies the branch length by the branch weight to yield the torque given a branch that ends in a weight" (branch-torque (make-branch 1 2)) 2)

;|
;(2, |------|)
;    |      |
;  (3, 1)  (1, 4)
(test "multiples the branch length by the branch weight to correct torque given a branch that ends in a binary mobile" (branch-torque (make-branch 2 mobile0)) 10)

; Part D
; All that changes is we are using cons instead of list. So we just need to change the places that use cadr because now cdr is sufficient.

(define branch-structure cdr)
(define right-branch cdr)