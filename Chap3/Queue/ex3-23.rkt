#lang sicp
(#%require "Queue.rkt")

; Exercise 3.23:
; A deque (“double-ended queue”) is a sequence in which items can be inserted and deleted at either the front or the rear.
; Operations on deques are the constructor make-deque, 
; the predicate empty-deque?, 
; selectors front-deque and rear-deque, 
; mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!.
; Show how to represent deques using pairs, and give implementations of the operations.
; All operations should be accomplished in Θ(1) steps.

(define (make-deque)
    (let ((queue (cons '() '())))
        
        (define (empty-deque?) (equal? queue (cons '() '())))
             
             
        (define (print-deque)
            (define (print ptr)
                (if (or (equal? ptr '()) (equal? ptr 'END))
                    (newline)
                    (begin (display (car ptr)) (display " ")
                           (print (caddr ptr)))))
            (print (front-deque)))
        
        ; SELECTOR
        (define (front-deque) (car queue))
        (define (rear-deque) (cdr queue))
        
        ; MUTATORS
        (define (front-insert-deque! item)
            (if (empty-deque?)
                (let ((new-pair (list item '() '())))
                     (set-car! queue new-pair)
                     (set-cdr! queue new-pair)
                     (print-deque))
                (let ((new-pair (list item '() (front-deque))))
                     (set-car! (cdr (front-deque)) new-pair)
                     (set-car! queue new-pair)
                     (print-deque))))
                    
            
        (define (rear-insert-deque! item)
            (if (empty-deque?)
                (let ((new-pair (list item '() '())))
                     (set-car! queue new-pair)
                     (set-cdr! queue new-pair)
                     (print-deque))
                (let ((new-pair (list item (rear-deque) '())))
                     (set-cdr! (cdr (rear-deque)) (list new-pair))
                     (set-cdr! queue new-pair)
                     (print-deque))))
        
        (define (front-delete-deque!)
            (if (empty-deque?)
                (error "<front-delete-deque!> delete deque with EMPTY QUEUE")
                (begin (set-car! queue (caddr (front-deque)))                        
                       (print-deque))))
        
        (define (rear-delete-deque!)
            (if (empty-deque?)
                (error "<rear-delete-deque!> delete deque with EMPTY QUEUE")
                (begin (set-cdr! queue (cadr (rear-deque)))
                       (set-cdr! (cdr (rear-deque)) '(END)) ; LISP 컴파일러가 최적화해버려서 '() 가 아니라 그냥 사라짐
                       (print-deque))))
        
        (define (dispatch m)
            (cond ((eq? m 'empty-deque?) empty-deque?)
                  ((eq? m 'front-deque) front-deque)
                  ((eq? m 'rear-deque) rear-deque)
                  ((eq? m 'front-insert-deque!) front-insert-deque!)
                  ((eq? m 'rear-insert-deque!) rear-insert-deque!)
                  ((eq? m 'front-delete-deque!) (front-delete-deque!))
                  ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
                  (else (error "<dispatch> No matching messages" (list m)))))
              
        dispatch))



(define x (make-deque))
((x 'front-insert-deque!) 'a)
((x 'front-insert-deque!) 'b)
((x 'front-insert-deque!) 'c)
((x 'rear-insert-deque!) 'd)
((x 'rear-insert-deque!) 'e)
((x 'rear-insert-deque!) 'f)
(x 'front-delete-deque!)
(x 'front-delete-deque!)
(x 'rear-delete-deque!)
(x 'rear-delete-deque!)