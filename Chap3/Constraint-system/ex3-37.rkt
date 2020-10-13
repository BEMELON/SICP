#lang sicp
(#%require "logics.rkt")

; Exercise 3.37
; The celsius-fahrenheit-converter procedure is cumbersome when compared with a more expressionoriented style of definition,

; such as

; (define (celsius-fahrenheit-converter x)
    ; (c+ (c* (c/ (cv 9) (cv 5))
    ;         x)
    ;     (cv 32)))
; (define C (make-connector))
; (define F (celsius-fahrenheit-converter C))

; Here c+, c*, etc. are the “constraint” versions of the arithmetic operations. 
; For example, c+ takes two connectors as arguments and returns a connector
; that is related to these by an adder constraint:

; (define (c+ x y)
     ;(let ((z (make-connector)))
       ;(adder x y z)
;       z))

; Define analogous procedures c-, c*, c/, and cv (constant value) that enable us to define compound constraints 
; as in the converter example above.

(define (cv C)
    (let ((z (make-connector)))
        (constant 9 z)
        z))
    
(define (c+ x y)
    (let ((z (make-connector)))
         (adder x y z)
         z))
     
(define (c- x y)
    (let ((z (make-connector)))
         (adder z y x)
         x))
     
(define (c* x y)
    (let ((z (make-connector)))
         (multiplier x y z)
         z))
     
(define (c/ x y)
   (let ((z (make-connector)))
        (multiplier z y x)
        x))
    
(define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))
    
; (define C (make-connector))
; (define F (celsius-fahrenheit-converter C))
; (probe 'C C)
; (probe 'F F)
; (set-value! C 35 'C)
; (forget-value! C 'C)
; (forget-value! F 'F)
; (set-value! F 324 'F)