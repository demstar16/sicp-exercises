#lang racket

(require "../test-harness.rkt")
(require "./paint-utils.rkt")
(require sicp-pict)

; ------------------ part a ------------------
(define outline-segments
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.0 0.99))
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.99 0.0))
  (make-segment
   (make-vect 0.99 0.0)
   (make-vect 0.99 0.99))
  (make-segment
   (make-vect 0.0 0.99)
   (make-vect 0.99 0.99))))

(define outline (segments->painter outline-segments))

(paint-to-png outline "chapter2/2.49a.png")

; --------------- part b ------------------------

(define cross-segments
    (list 
        (make-segment
            (make-vect 0.0 0.0)
            (make-vect 1.0 1.0))
        (make-segment 
            (make-vect 0.0 1.0)
            (make-vect 1.0 0.0))))

(define cross (segments->painter cross-segments))

(paint-to-png cross "chapter2/2.49b.png")

; -------------- part c -----------------------------

(define diamond-segments
    (list
        (make-segment
            (make-vect 0.0 0.5)
            (make-vect 0.5 1.0))
        (make-segment
            (make-vect 0.5 1.0)
            (make-vect 1.0 0.5))
        (make-segment
            (make-vect 1.0 0.5)
            (make-vect 0.5 0.0))
        (make-segment
            (make-vect 0.5 0.0)
            (make-vect 0.0 0.5))))

(define diamond (segments->painter diamond-segments))

(paint-to-png diamond "chapter2/2.49c.png")

; ------------- part d ----------------------

(define wave-segments
    (list
        (make-segment
            (make-vect 0.25 0.9)
            (make-vect 0.45 1.0))
        (make-segment
            (make-vect 0.65 1)
            (make-vect 0.75 0.9))
        (make-segment
            (make-vect 0.75 0.9)
            (make-vect 0.65 0.6))
        (make-segment 
            (make-vect 0.65 0.6)
            (make-vect 0.8 0.6))
        (make-segment
            (make-vect 0.8 0.6)
            (make-vect 1.0 0.4))
        (make-segment 
            (make-vect 1.0 0.2)
            (make-vect 0.65 0.45))
        (make-segment
            (make-vect 0.65 0.45)
            (make-vect 0.75 0.0))
        (make-segment 
            (make-vect 0.6 0.0)
            (make-vect 0.5 0.35))
        (make-segment 
            (make-vect 0.5 0.35)
            (make-vect 0.3 0.0))
        (make-segment
            (make-vect 0.15 0)
            (make-vect 0.4 0.45))
        (make-segment 
            (make-vect 0.4 0.45)
            (make-vect 0.25 0.6))
        (make-segment 
            (make-vect 0.25 0.6)
            (make-vect 0.15 0.45))
        (make-segment 
            (make-vect 0.15 0.45)
            (make-vect 0.0 0.5))
        (make-segment
            (make-vect 0.0 0.7)
            (make-vect 0.15 0.55))
        (make-segment 
            (make-vect 0.15 0.55)
            (make-vect 0.25 0.7))
        (make-segment
            (make-vect 0.25 0.7)
            (make-vect 0.45 0.6))
        (make-segment 
            (make-vect 0.45 0.6)
            (make-vect 0.25 0.9))))

(define wave (segments->painter wave-segments))

(paint-to-png wave "chapter2/2.49d.png")