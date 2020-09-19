#lang sicp
(#%provide make-queue 
           rear-ptr front-ptr empty-queue? front-queue
           set-front-ptr! set-rear-ptr! insert-queue! delete-queue!)

; contructor
(define (make-queue) (cons '() '()))

; selector  
(define (rear-ptr q) (cdr q))

(define (front-ptr q) (car q))

(define (empty-queue? q) (null? (front-ptr q)))

(define (front-queue q)
    (if (empty-queue? q)
        (error "<front-queue> Front-queue called with an empty queue")
        (car (front-ptr q))))
    
; mutator
(define (set-front-ptr! q item) (set-car! q item))

(define (set-rear-ptr! q item) (set-cdr! q item))

(define (insert-queue! q item)
    (let ((new-pair (cons item '())))
         (cond ((empty-queue? q) (begin
                                     (set-front-ptr! q new-pair)
                                     (set-rear-ptr! q new-pair)
                                      q))
               (else 
                     (begin 
                            (set-cdr! (rear-ptr q) new-pair)
                            (set-rear-ptr! q new-pair)
                            q)))))
                        
(define (delete-queue! q)
    (cond ((empty-queue? q) (error "<delete-queue!> DELETE! called with an empty queue"))
          (else 
                (begin
                    (set-front-ptr! q (cdr (front-ptr q)))
                    q))))
                