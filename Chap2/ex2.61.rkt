#lang sicp


; Ordered List
(define (adjoin-set x set)
    (cond ((null? set) x)
          ((eq? x (car set)) (cons x set))
          ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
          (else (cons x set))))
      
(adjoin-set 0 (list 1 2 3 5))

        