#lang sicp

; Exercise 3.18
; Write a procedure that examines a list and determines whether it contains a cycle, 
; that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. 
; Exercise 3.13 constructed such lists

(define (loop? items)
    (define (exist? x items)
        (cond ((null? items) #f)
              ((eq? (car items) x) #t)
              (else (exist? x (cdr items)))))
    
    (let ((pool '()))
         (define (check-loop items)
                  (if (null? items) 
                      #f
                    (let ((item (car items)))               
                        (cond ((exist? item pool) #t)
                              ((null? item) #f)
                              (else 
                                (begin 
                                    (set! pool (append pool (list item)))
                                    (check-loop (cdr items))))))))
         (check-loop items)))
     

(define l1 (list 'a 'b 'c))
(define l2 (list 'a 'b 'c))
(set-cdr! (cdr (cdr l2)) l2)
(define l3 (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr l3) (cdr l3))
(define l4 (list 'a 'b 'c 'd 'e))
(set-car! (cdddr l4) (cddr l4))

(loop? l2) ; #t
(loop? l3) ; #t
(loop? l4) ; #f