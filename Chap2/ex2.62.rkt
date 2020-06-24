#lang sicp

; Ordered List
(define (union-set s1 s2)
    (cond ((and (not (null? s1)) (null? s2)) s1)
          ((and (not (null? s2)) (null? s1)) s2)
          ((and (null? s1) (null? s2)) '())
          ((eq? (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
          ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))
          ((> (car s2) (car s1)) (cons (car s1) (union-set (cdr s1) s2)))))
      
      
(define l1 (list 1 3 5 7 9))
(define l2 (list 2 4 6 8 10))
(define l3 (list 1 2 3 5 6 7 9))

(union-set l1 l2)
(union-set l1 l3)