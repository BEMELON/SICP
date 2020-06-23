#lang sicp
(#%provide element-of-set adjoin-set intersection-set)

(define (element-of-set object s)
    (cond ((null? s) #f)
          ((eq? (car s) object) #t)
          (else (element-of-set object (cdr s)))))
      
(define (adjoin-set object s)
    (if (element-of-set object s)
        s
        (cons s object)))
    
(define (intersection-set s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((element-of-set (car s1) s2) (cons s1 (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))))
      
            
    
        