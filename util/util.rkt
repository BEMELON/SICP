#lang sicp
(#%require (only racket/base current-inexact-milliseconds)
           (only racket 
                 make-hash
                 hash-set!
                 hash-ref!))


(#%provide runtime put get type-tag contents apply-generic)

; Provide Runtime 
(define (runtime) (current-inexact-milliseconds))


; PUT GET for Hash Table
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref! *op-table* (list op type) '()))

; Functions for Generic-apply
(define (type-tag object) (car object))
(define (contents object) (cdr object)) 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
 (let ((proc (get op type-tags)))
   (if proc
         (apply proc (map contents args))
         (error "No method for There Types: APPLY-GENERIC" (list op type-tags))))))