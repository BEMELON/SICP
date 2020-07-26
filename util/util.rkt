#lang sicp
(#%require (only racket/base current-inexact-milliseconds)
           (only racket 
                 make-hash
                 hash-set!
                 hash-ref!))


(#%provide runtime put get type-tag contents apply-generic attach-tag square)

; Provide Runtime 
(define (runtime) (current-inexact-milliseconds))
(define (square x) (* x x))

; PUT GET for Hash Table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref! *op-table* (list op type) '()))


; ===========================================================================
; Exercise 2.78 : Update functions for Ordinary numbers
; ===========================================================================
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
    
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
    (if proc
            (apply proc (map contents args))
            (error "No method for There Types: APPLY-GENERIC" (list op type-tags))))))