#lang sicp
(#%require "../util/util.rkt")

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
      
;   ===========================================================================
;   Exercise 2.83
;   ===========================================================================
(define (raise datum)
   (define (get-upper-type type)
       (let ((level (get-tower-level-by-type type)))
             (get-tower-type-by-level (+ 1 level))))
                   
   (let ((type (type-tag datum))
         (content (contents datum)))
     (let ((upper_type (get-upper-type type)))
         (let ((proc (get 'raise type)))
               (if (not-null? proc)
                   (proc content)
                   (error "<raise> no matching coercion proc for these types : " (list type upper_type)))))))

        