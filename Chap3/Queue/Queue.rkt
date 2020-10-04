#lang sicp
(#%provide make-queue rear-ptr front-ptr empty-queue? front-queue
           set-front-ptr! set-rear-ptr! insert-queue! delete-queue!)

(define (rear-ptr q) (q 'rear-ptr))
(define (front-ptr q) (q 'front-ptr))
(define (empty-queue? q) ((q 'empty-queue?)))
(define (front-queue q) (q 'front-queue))
(define (set-front-ptr! q item) ((q 'set-front-ptr!) item))
(define (set-rear-ptr! q item) ((q 'set-front-ptr!) item))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) (q 'delete-queue!))

; CONSTRUCTOR
; Exercise 3.22
; Instead of representing a queue as a pair of pointers, 
; we can build a queue as a procedure with local state. 
; The local state will consist of pointers to the beginning and the end of an ordinary list. 
; Thus, the make-queue procedure will have the form
(define (make-queue)
    (let ((front-ptr (cons '() '()))
          (rear-ptr (cons '() '())))
      
      (define (print-queue)
          (define (print ptr)
              (if (null? ptr)
                  (newline)
                  (begin (display (car ptr)) (display " ")
                         (print (cdr ptr)))))
         ;(display "QUEUE : ") (print front-ptr))
         (display ""))
      
      ; SELECTOR
      (define (empty-queue?) 
          (or (equal? front-ptr '())
              (equal? front-ptr (cons '() '()))
              (equal? front-ptr '(()))))
                                  
      (define (front-queue)
          (if (empty-queue?)
              (error "<front-queue> Front-queue called with an empty queue" (print-queue))
              (car front-ptr)))
          
      ; MUTATOR
      (define (set-front-ptr! item) (set! front-ptr item))
    
      (define (set-rear-ptr! item) (set! rear-ptr item))
    
      (define (insert-queue! item)
          (let ((new-pair (cons item '())))
               (cond ((empty-queue?) (begin
                                           (set-front-ptr! new-pair)
                                           (set-rear-ptr! new-pair)
                                           (print-queue)))
                     (else (begin 
                                  (set-cdr! rear-ptr new-pair)
                                  (set-rear-ptr! new-pair)
                                  (print-queue))))))
                            
      (define (delete-queue!)
          (cond ((empty-queue?) (error "<delete-queue!> DELETE! called with an empty queue" (print-queue)))
                (else 
                      (let ((item (car front-ptr)))
                          (set-front-ptr! (cdr front-ptr))
                          item))))
                          
      

      
      (define (dispatch m)
          (cond ((eq? m 'rear-ptr) rear-ptr)
                ((eq? m 'front-ptr) front-ptr)
                ((eq? m 'empty-queue?) empty-queue?)
                ((eq? m 'front-queue) front-queue)
                ((eq? m 'set-front-ptr!) set-front-ptr!)
                ((eq? m 'set-rear-ptr!) set-rear-ptr!)
                ((eq? m 'insert-queue!) insert-queue!)
                ((eq? m 'delete-queue!) (delete-queue!))
                (else (error "<dispatch> No matching messages" (list m)))))
      dispatch))

