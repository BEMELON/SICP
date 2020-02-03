#lang sicp
(#%require "Equation_Newton-method.rkt")

(define (cubic a b c)
  (lambda (x) (+
               (* x x x)
               (* a x x)
               (* b x)
               c)))

(define (equation a b c)
  (newtons-method (cubic a b c) 1))

(equation 2 4 3)