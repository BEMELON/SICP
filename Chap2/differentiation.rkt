#lang sicp
(#%provide make-sum make-product
           variable? same-variable? =number?
           sum? addend augend
           product? multiplier multiplicand
           deriv)

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) 
              (accumulate op init (cdr seq)))))

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
    ;(display "[DBG] <make-sum> t1 |=> ") (display t1) (newline)
    ;(display "[DBG] <make-sum> t2 |=> ") (display t2) (newline)
    (cond  ((=number? t1 0) t2)
           ((=number? t2 0) t1)
           ((and (number? t1) (number? t2)) (+ t1 t2))
           (else (list '+ t1 t2))))
                      
                     
; Construct the product of m1 and m2.
(define (make-product t1 t2)
    ;(display "[DBG] <make-product> t1 |=> ") (display t1) (newline)
    ;(display "[DBG] <make-product> t2 |=> ") (display t2) (newline)
    (cond  ((or (=number? t1 0) (=number? t2 0)) 0)
           ((=number? t1 1) t2)
           ((=number? t2 1) t1)
           ((and (number? t1) (number? t2)) (* t1 t2))
           (else (list '* t1 t2))))

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
    (if  (> (length t) 3)
         (cons '+ (cddr t))
         (caddr t)))
    

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
    (if (> (length t) 3)
        (cons '* (cddr t))
        (caddr t)))


(define (deriv exp var)
    (display "[DBG] <deriv> exp |=> ") (display exp) (newline)
    (cond ((number? exp) 0) 
          ((variable? exp) 
                      (if (same-variable? exp var) 1 0))
          ((sum? exp) 
                      (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp)
                      (make-sum
                            (make-product   (multiplier exp)
                                            (deriv (multiplicand exp) var))
                            (make-product   (deriv (multiplier exp) var)
                                            (multiplicand exp))))
          (else
            (error "unknown expression type: DERIV" exp))))
        
        