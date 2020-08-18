#lang sicp
(#%require (only racket/base current-inexact-milliseconds)
           (only racket 
                 make-hash
                 hash-set!
                 hash-ref!))


(#%provide runtime square not-null?
           put get 
           put-coercion get-coercion
           type-tag contents attach-tag
           flatmap)

; Provide Runtime 
(define (runtime) (current-inexact-milliseconds))
(define (square x) (* x x))
(define (not-null? object) (not (null? object)))

(define (accumulate proc init seq)
    (if (null? seq)
        init
        (append (proc (car seq))
                (accumulate proc init (cdr seq)))))
            
(define (flatmap proc seq)
    (if (pair? seq)
        (accumulate append nil (map proc seq))
        seq))

; ===========================================================================
; Hash Table operations 
; ===========================================================================
(define *op-table* (make-hash))

(define (put op type proc)
    ;(display op) (display " / ") (display type) (display " / ") (display proc) (newline)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
    ;   (display op) (display " / ") (display type) (display " / ") 
    ;   (display (hash-ref! *op-table* (list op type) '())) (newline)
  (hash-ref! *op-table* (list op type) '()))

(define *coercion-table* (make-hash))

(define (put-coercion to be proc)
    (hash-set! *coercion-table* (list to be) proc))

(define (get-coercion to be)
    (hash-ref! *coercion-table* (list to be) '()))

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
    (cons type-tag datum))
    