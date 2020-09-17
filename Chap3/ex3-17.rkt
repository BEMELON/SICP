#lang simply-scheme

; Exercise 3.17
; Devise a correct version of the count-pairs procedure of Exercise 3.16 
; that returns the number of distinct pairs in any structure. (Hint: Traverse the structure,
; maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

(define (count-pairs x)
    (let ((registered-pairs '()))
        (define (counted? x items)
            (cond ((null? items) #f)
                  ((eq? x (car items)) #t)
                  (else (counted? x (cdr items)))))
                    
        (define (pairs x)
            (if (or (not (pair? x)) (counted? x registered-pairs)) 
                0
                (begin
                    (set! registered-pairs (append registered-pairs x))
                    (+ (pairs (car x))
                       (pairs (cdr x))
                       1))))
        (pairs x)))



(count-pairs (list 'a 'b 'c)) ;3

(define a (list 'a 'b 'c))
(count-pairs (cons a a)) ;1

(define b (list 'a 'b))
(count-pairs (append a b)) ;3
