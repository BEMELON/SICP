#lang sicp
(#%require "logics.rkt")

; Exercise 3.33
; Using primitive multiplier, adder, and constant constraints, 
; define a procedure averager that takes three connectors a, b, and c as inputs 
; and establishes the constraint that the value of c is the average of the values of a and b

(define (averager a b c)
    (let ((temp (make-connector))
          (count (make-connector)))
         (constant 2 count)
         (adder a b temp)
         (multiplier c count temp)
         (display "<averager> ok\n")))
    