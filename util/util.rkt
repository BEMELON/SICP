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
        (let ((level (get-tower-level-by-type type)))
             (get-tower-type-by-level (+ 1 level))))
                     
    (let ((type (type-tag datum))
          (content (contents datum)))
      (let ((upper_type (get-upper-type type)))
           (let ((proc (get-coercion type upper_type)))
                (if (not-null? proc)
                    (proc datum)
                    (error "<raise> no matching coercion proc for these types : " (list datum upper_type)))))))
                
(define (get-tower-level-by-type type)
    (cond ((eq? type 'scheme-number) 1)
          ((eq? type 'rational) 2)
          ((eq? type 'real) 3)
          ((eq? type 'complex) 4)
          (else (error "<get-tower-level-by-type> no matching type in type-tower : " (list type)))))
      
(define (get-tower-type-by-level level)
    (cond ((eq? level 1) 'scheme-number)
          ((eq? level 2) 'rational)
          ((eq? level 3) 'real)
          ((eq? level 4) 'complex)
          (else (error "<get-tower-type-by-level> no matching level in type-tower : " (list level)))))
                
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
                 
    ; =======================================================================
    ; Exercise 2.84
    ; =======================================================================
    (define (find-highest-type type-tags)
        (define (search type-tags result)
            (if (null? type-tags)
                result
                (let ((level (get-tower-level-by-type (car type-tags))))
                     (if (< result level)
                         (search (cdr type-tags) level)
                         (search (cdr type-tags) result)))))
        (get-tower-type-by-level (search type-tags 0)))
                                               

    (define (convert target args)
        (define (raise-until-target target arg)
            (if (eq? target (type-tag arg))
                arg
                (raise-until-target target (raise arg))))
            
        (define (raise-args args)
            (if (null? args)
                nil
                (cons (raise-until-target target (car args))
                      (raise-args (cdr args)))))
        (raise-args args))

    ; =======================================================================
    ; coercion - convert
    ; =======================================================================
    ; (define (convert target args)
    ;     (define (toTarget arg)
    ;         (let ((type-tag (type-tag arg)))
    ;             (let ((proc (get-coercion type-tag target)))
    ;                 (if (not-null? proc)
    ;                     (proc arg)
    ;                     arg))))
    ;     (map toTarget args))

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
                (let ((target (find-highest-type type-tags)))
                     (display "    <apply-generic> target : ") (display target) (newline)
                    (if (not-null? target)
                        (apply apply-generic (append (list op) (convert target args)))
                        (error "No method for These Types: APPLY-GENERIC" (list op type-tags))))))))