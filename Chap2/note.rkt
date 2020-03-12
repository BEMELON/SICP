#lang sicp
(#%require "Rational.rkt")

(define one-third (make-rat 1 3))
(define two-five (make-rat 2 5))
(print-rat (add-rat one-third two-five))