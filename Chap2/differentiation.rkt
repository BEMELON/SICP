#lang sicp
(#%provide make-sum make-product
           variable? same-variable?
           sum? addend augend
           product? multiplier multiplicand
           deriv)

; Is exp number? and exp == var?       
(define (=number? exp var)
    (and (number? exp) (= exp var)))

; Is e a variable?
(define (variable? e) (symbol? e))

; Are v1 and v2 the same variable?
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2)
         (eq? v1 v2)))
     
; Construct the sum of a1 and a2.
(define (make-sum t1 t2)
    (cond  ((=number? t1 0) t2)
           ((=number? t2 0) t1)
           ((and (number? t1) (number? t2)) (+ t1 t2))
           (else (list '+ t1 t2))))
                      
                     
; Construct the product of m1 and m2.
(define (make-product t1 t2)
    (cond  ((or (=number? t1 0) (=number? t2 0)) 0)
           ((=number? t1 1) t2)
           ((=number? t2 1) t1)
           ((and (number? t1) (number? t2)) (* t1 t2))
           (else (list '(* t1 t2)))))

; Is e a sum?
(define (sum? t)
    (if (and (pair? t)
             (eq? (car t) '+))
        #t
        #f))

; Second item of the sum list
(define (addend t)
    (cadr t))

; Third item of the sum list
(define (augend t)
    (caddr t))

; Is e a product?
(define (product? t)
    (if (and (pair? t)
             (eq? (car t) '*))
         #t
         #f))
     
; Multiplier of the product t.
(define (multiplier t)
    (cadr t))

; Multiplicand of the product t.
(define (multiplicand t)
    (caddr t))


(define (deriv exp var)
    (cond ((number? exp) 0) 
          ((variable? exp) 
                  (if (same-variable? exp var) 1 0))
          ((sum? exp) 
                 (make-sum (deriv (addend exp) var)
                          (deriv (augend exp) var)))
          ((product? exp)
                      (make-sum
                        (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
          (else
            (error "unknown expression type: DERIV" exp))))
        
        
(deriv '(+ x 3) 'x)