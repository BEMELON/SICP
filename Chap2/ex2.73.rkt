#lang sicp 
(#%require "../util/util.rkt")

(define (deriv exp var)
    (define (variable? exp) (symbol? exp))
    (define (same-variable? exp var) (eq? exp var))
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))
    (cond   ((number? exp) 0)
            ((variable? exp) (if (same-variable? exp var) 1 0))
            (else ((get 'deriv (operator exp)) (operands exp) var))))
        
(define (install-sum-package)
    ;; internal procedures
    (define (make-sum pre post)
        (if (and (number? pre) (number? post)) 
            (+ pre post)
            (list '+ pre post)))
    (define (addend exp) (car exp))
    (define (augend exp) (cadr exp))
    (define (deriv-sum exp var) (make-sum (deriv (addend exp) var)
                                          (deriv (augend exp) var)))
    
    ;; interface
    (put 'make-sum '+ make-sum)
    (put 'deriv '+ deriv-sum)
    '(install-sum-package done))

(define (install-product-package)
    ;;internal procdeures
    (define (=number? exp var) (and (number? exp) (eq? exp var)))
    (define (make-product pre post) 
        (cond ((=number? pre 1) post)
              ((=number? post 1) pre)
              ((and (number? pre) (number? post)) (* post pre))
              ((or (=number? pre 0) (=number? pre 0)) 0)
              (else (list '* pre post))))
    (define (multiplier exp) (car exp))
    (define (multiplicand exp) (cadr exp))
    (define (deriv-product exp var) ((get 'make-sum '+) 
                                            (make-product (deriv (multiplier exp) exp) (multiplicand exp))
                                            (make-product (multiplier exp) (deriv (multiplicand exp) var))))
    
    ;; interface
    (put 'make-product '* make-product)
    (put 'deriv '* deriv-product)
    '(install-product-package done))

(install-sum-package)
(install-product-package)
(deriv '(+ x 3 x) 'x)
(deriv '(* 0 x) 'x)
    