#lang sicp

; Non-duplicate : O((len set))
; dpulicate : O((len set))
(define (element-of-set x set)
    (cond ((null? set) #f)
          ((eq? x (car set)) #t)
          (else (element-of-set x (cdr set)))))

; Non-duplicate : O((len set))
; dpulicate : O(1)
(define (adjoin-set x set)
    (cons x set))

; Non-duplicate : O((max (len s1) (len s2)))
; dpulicate : O(1)
(define (union-set s1 s2)
    (append s1 s2))

; Non-duplicate : O((len s1)^2)
; dpulicate : O((len s1)^2)
(define (intersection-set s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((element-of-set (car s1) s2) (cons (car s1) (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))))
      
(define s1 (list 1 2 3 2 3 1 4))
(define s2 (list 3 4 5 3 3 4 5))

(element-of-set 1 s1)
(adjoin-set 10 s1)
(union-set s1 s2)
(intersection-set s1 s2)