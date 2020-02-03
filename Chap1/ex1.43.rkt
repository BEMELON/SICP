#lang sicp
(#%require "ex1.42.rkt")
(#%provide repeated)

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (repeated (compose f f) (- n 1))))

(define (square x) (* x x))

;((repeated square 2) 5)