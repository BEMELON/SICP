#lang sicp
(#%require "differentiation.rkt")


(define (deriv exp var)
    (cond ((number? exp) 0) 
          ((variable? exp) 
                  (if (same-variable? exp var) 1 0))
          ((sum? exp) 
                 (make-sum  (deriv (addend exp) var)
                            (deriv (augend exp) var)))
          ((product? exp)
                 (make-sum
                            (make-product   (multiplier exp)
                                            (deriv (multiplicand exp) var))
                            (make-product   (deriv (multiplier exp) var)
                                            (multiplicand exp))))
          ((exponentiation? exp) 
                 (make-product  (exponentiation exp)
                                (make-exponentiation    (base exp)
                                                        (- (exponentiation exp) 1))))
                            
                        
          (else
            (error "unknown expression type: DERIV" exp))))

(define (base expression) (cadr expression))
    
(define (exponentiation expression) (caddr expression))

(define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))

(define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else (list '** base exp))))
    
(deriv '(** x 3) 'x)