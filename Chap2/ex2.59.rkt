#lang sicp
(#%require "set.rkt")

(define (union-set s1 s2)
    (cond ((or (null? s1) (null? s2)) s2)
          ((not (element-of-set (car s1) s2)) (union-set (cdr s1) (cons (car s1) s2)))
          (else (union-set (cdr s1) s2))))
      
(union-set (list 1 2 3) (list 3 4 5))

          
    