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
           apply-generic)

; Provide Runtime 
(define (runtime) (current-inexact-milliseconds))
(define (square x) (* x x))
(define (not-null? object) (not (null? object)))

; ===========================================================================
; Hash Table operations 
; ===========================================================================
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
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
    (cond ((eq? type-tag 'scheme-number) datum)
          (else (cons type-tag datum))))
    
    
; ===========================================================================
; Exercise 2.83
; ===========================================================================
(define (raise datum)
    (define (get-upper-type type)
        (cond ((eq? type 'scheme-number) 'rational)
              ((eq? type 'rational) 'real)
              ((eq? type 'real) 'complex)
              (else '())))
    (let ((type (type-tag datum))
          (content (contents datum)))
      (let ((upper_type (get-upper-type type)))
           (let ((proc (get-coercion type upper_type)))
                (if (not-null? proc)
                    (proc datum upper_type)
                    (error "<raise> no matching coercion proc for these types : " (list datum upper_type)))))))
                
; ===========================================================================
; apply-generic : Multi operands compose of different types
; ===========================================================================        
(define (apply-generic op . args)
    ; =======================================================================
    ; internal fucntions
    ; =======================================================================
    (define (not-null? x) (not (null? x)))
    
    ; match? target , type-tag
    (define (find-type target type-tags args)
        (if (null? type-tags)
            target
            (let ((type1 (car type-tags))
                  (arg1 (car args)))
              (let ((target-coercion (get-coercion type1 target)))
                   (if (not-null? target-coercion)
                       (find-type target (cdr type-tags) (cdr args))
                       nil)))))
    
    ; Target looper
    (define (find-target type-tags args)
        (display "             <find-target> type-tags : ") (display type-tags) (newline)
        (display "             <find-target> args : ") (display args) (newline)
        (if (null? type-tags) 
            nil
            (let ((target (find-type (car type-tags) (cdr type-tags) (cdr args))))
                 (if (null? target) 
                     (find-target (cdr type-tags) (cdr args))
                     target))))

    (define (convert target args)
        (define (toTarget arg)
            (let ((type-tag (type-tag arg)))
                (let ((proc (get-coercion type-tag target)))
                    (if (not-null? proc)
                        (proc arg)
                        arg))))
        (map toTarget args))

    ; =======================================================================
    ; procedures
    ; =======================================================================
    (display "[DBG][START] apply-generic\n")
    (display "    <apply-generic> op : ") (display op) (newline)
    (display "    <apply-generic> args : ") (display args) (newline)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (display "    <apply-generic> type-tags : ") (display type-tags) (newline)
            (display "    <apply-generic> proc : ") (display proc) (newline)
            (if (not-null? proc)    
                (apply proc (map contents args))
                (let ((target (find-target type-tags args)))
                     (display "    <apply-generic> target : ") (display target) (newline)
                    (if (not-null? target)
                        (apply apply-generic (append (list op) (convert target args)))
                        (error "No method for These Types: APPLY-GENERIC" (list op type-tags))))))))