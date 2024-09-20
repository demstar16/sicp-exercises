#lang racket

(require "../test-harness.rkt")
(require "../utils.rkt")

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(test "d/dx(x^2)" (deriv '(** x 2) 'x) '(* 2 x))
(test "d/dx(ax^2)" (deriv '(* a (** x 2)) 'x) '(* a (* 2 x)))
(test "d/dx(ax^2 + bx)" (deriv '(+ (* a (** x 2)) (* b x)) 'x) '(+ (* a (* 2 x)) b))
(test "d/dx(ax^2 + bx + c)" (deriv '(+ (+ (* a (** x 2)) (* b x)) c) 'x) '(+ (* a (* 2 x)) b))