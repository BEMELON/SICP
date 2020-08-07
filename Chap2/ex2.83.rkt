#lang sicp
(#%require "../util/util.rkt")


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
            
        