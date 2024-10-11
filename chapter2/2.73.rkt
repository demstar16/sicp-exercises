#lang racket

(require "../test-harness.rkt")
(require "../utils.rkt")

; a.) number? and variable? can't be assimilated because we are using the operand as the type in the table and deriv as the operator. If we try and use a single number or variable in the dispatch they don't have an operator prefix and our selectors oeprator and operand won't work as intended.

; b.)
(define (install-sum-product-package)
    (define (augend exp) (cadr exp))
    (define (addend exp) (caddr exp))
    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2))
            (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))

    (define (multiplicand exp) (cadr exp)) 
    (define (multiplier exp) (caddr exp))
    (define (make-product m1 m2)
        (cond ((or (=number? m1 0)
                (=number? m2 0))
            0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2))
            (* m1 m2))
            (else (list '* m1 m2))))
    (define (deriv-product exp var)
       (make-sum
        (make-product (multiplier exp) (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var) (multiplicand exp))))

    (put 'deriv '+ deriv-sum)
    (put 'deriv '* deriv-product)
    'done
)

; c.) 
(define (install-exponentiation-package)
    (define (augend exp) (cadr exp))
    (define (addend exp) (caddr exp))
    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
              ((=number? a2 0) a1)
              ((and (number? a1) (number? a2)) (+ a1 a2))
              (else (list '+ a1 a2))))
    (define (multiplicand exp) (cadr exp)) 
    (define (multiplier exp) (caddr exp))
    (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
              ((=number? m1 1) m2)
              ((=number? m2 1) m1)
              ((and (number? m1) (number? m2)) (* m1 m2))
              (else (list '* m1 m2))))
    (define (base e) (cadr e))
    (define (exponent e) (caddr e))
    (define (make-exponentiation base exponent)
        (cond ((=number? exponent 0) 1)
            ((=number? exponent 1) base)
            ((and (number? base) (number? exponent)) (expt base exponent))
            (else (list '** base exponent))))
    (define (deriv-exponentiation exp var) 
        (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))

    (put 'deriv '** deriv-exponentiation)
    'done
)

; d.) Since all that has changed is the order we can simply just flip the way we put data into the table.
(put '** 'deriv deriv-exponentiation)