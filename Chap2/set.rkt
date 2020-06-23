#lang sicp
(#%provide element-of-set adjoin-set intersection-set)

(define (element-of-set s object)
    (cond ((null? s) #f)
          ((eq? (car s) object) #t)
          (else (element-of-set (cdr s) object))))
      
(define (adjoin-set s object)
    (if (element-of-set s object)
        s
        (cons s object)))
    
(define (intersection-set s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((element-of-set (car s1) s2) (cons s1 (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))))
      
            
    
        