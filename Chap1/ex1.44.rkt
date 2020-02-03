#lang sicp
(#%require "ex1.43.rkt")

(define dx 0.0001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  (repeated smooth n) f)

(define (cube x) (* x x x))

(n-fold-smooth cube 5)
  