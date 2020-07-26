#lang sicp


(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((pair? datum) (car datum))
          (else (error "[ERROR]<type-tag> Bad type ----" datum))))

(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "[ERROR]<contents> Bad type ----" datum))))
    
(define (attach-tag type-tag datum)
    (cond ((eq? type-tag 'scheme-number) datum)
          (else (cons type-tag datum))))

