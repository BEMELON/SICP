#lang sicp

(define (filter proc seq)
	;(display "[DBG] <filter> seq |=> ") (display seq) (newline)
    (cond ((null? seq) nil)
          ((proc (car seq)) (cons (car seq) (filter proc (cdr seq))))
          (else (filter proc (cdr seq)))))
            
(define (accumulate op init seq)
	;(display "[DBG] <accumulate> seq |=> ") (display seq) (newline)
    (if (null? seq)
        init
        (op (car seq)
            (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
    (display "[DBG] <flatmap> seq |=> ") (display seq) (newline)
    (accumulate append nil (map proc seq)))
    
(define (map proc seq)
	;(display "[DBG] <map> seq |=> ") (display seq) (newline)
    (if (null? seq)
        '()
        (cons (proc (car seq)) (map proc (cdr seq)))))

(define (enumerate-interval n last)
    (if (> n last)
        nil
        (cons n (enumerate-interval (+ 1 n) last))))
           
        
        
(define empty-board nil)

(define (safe? k positions)
	(define (bishop-check positions)
		(define (check col target positions)
			;(display "[DBG] <bishop-check> positions |=> ") (display positions) (newline)
			;(display "[DBG] <bishop-check> target |=> ") (display target) (newline)
			;(display "[DBG] <bishop-check> col |=> ") (display col) (newline)
			(if (null? positions)
			    #true
			    (and (not (= (+ target col) (car positions)))
			         (not (= (- target col) (car positions)))
			         (check (+ 1 col) target (cdr positions)))))
			   
		;(display "[DBG] <main> <bishop-check> positions |=> ") (display positions) (newline)
		(if (null? positions)
		    #true
			(and (check 1 (car positions) (cdr positions))
			     (bishop-check (cdr positions)))))
			
		
		
	(define (rook-check positions)
		(define (check target positions)
			(if (null? positions)
			    #true
			    (and (not (= target (car positions))) (check target (cdr positions)))))
			   
		;(display "[DBG] <rook-check> positions |=> ") (display positions) (newline)
		(if (null? positions)
		    #true
		    (and (check (car positions) (cdr positions))
		         (rook-check (cdr positions)))))
		
	;(display "[DBG] <safe?> k |=> ") (display k) (newline)
    ;(display "[DBG] <safe?> positions |=> ") (display positions) (newline)
    (and (rook-check positions)
         (bishop-check positions)))
	
(define (adjoin-position new-row k rest-of-queens)
	;(display "[DBG] <adjoin-position> k |=> ") (display k) (newline)
    ;(display "[DBG] <adjoin-position> result |=> ") (display (cons new-row rest-of-queens)) (newline)
    (cons new-row rest-of-queens))
    
(define (queens board-size)
    (define (queen-cols k)
        ;(display "[DBG] <queen-cols?> k |=> ") (display k) (newline)
        (if (= k 0)
            ;(begin (display "[DBG] Return empty-board\n")
			(list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                ; rest-of-queens 에 new-row 를 append
                (flatmap
                    (lambda (rest-of-queens)
                        (map 
                            (lambda (new-row) (adjoin-position new-row k rest-of-queens))                          
                            (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))
 
(define (queens-Louis board-size)
	(define (queen-cols k)
		(if (= k 0)
		    (list empty-board)
		    (filter
	    		(lambda (positions) (safe? k positions))
	    		; new-row 에 rest-of-queens 를 append
	    		(flatmap
	    			(lambda (new-row)
	    				(map 
	    				     (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
	    				     (queen-cols (- k 1))))
				    (enumerate-interval 1 board-size)))))
	(queen-cols board-size))
 
;(safe? 3 (list 4 1 3))
(define result (queens 4))
(display result) (newline)
(display (length result))

