#lang sicp

(define (filter proc seq)
    (cond ((null? seq) nil)
          ((proc (car seq)) (cons (car seq) (filter proc (cdr seq))))
          (else (filter proc (cdr seq)))))
            
(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq)
            (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
    (display "SEQ : " ) (display seq) (newline)
    (accumulate append nil (map proc seq)))
    
(define (map proc seq)
    (if (null? seq)
        nil
        (cons (proc (car seq)) (map proc (cdr seq)))))

(define (enumerate-interval n last)
    (if (> n last)
        nil
        (cons n (enumerate-interval (+ 1 n) last))))
           
        
        
(define empty-board nil)

(define (safe? k positions)
    (display "[DBG] <safe?> positions |=> ") (display positions) (newline)
    #true)
    
(define (adjoin-position new-row k rest-of-queens)
    (display "[DBG] <adjoin-position> rest-of-queens |=> ") (display rest-of-queens) (newline)
    (append rest-of-queens (cons new-row k)))
    
(define (queens board-size)
    (define (queen-cols k)
        (display "[DBG] <queen-cols?> (START) k |=> ") (display k) (newline)
        (if (= k 0)
            empty-board
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map 
                            (lambda (new-row) (adjoin-position new-row k rest-of-queens))                          
                            (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1)))))
                    (display "[DBG] <queen-cols?> (END) k |=> ") (display k) (newline))
    (queen-cols board-size))

(queens 7)

                