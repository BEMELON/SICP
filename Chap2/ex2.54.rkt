#lang sicp
  
(define (precise-equal? x y)
    (define (flatmap seq)
        (cond   ((null? seq) '())
                ((pair? (car seq)) (append (flatmap (car seq)) (flatmap (cdr seq))))
                (else (append (list (car seq)) (flatmap (cdr seq))))))
            
    (let ((x (flatmap x))
          (y (flatmap y)))
          (equal? x y)))

(precise-equal? '(this is a list) '(this (is a) list))
      
      