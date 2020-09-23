#lang sicp

; Exercise 3.25
; Generalizing one- and two-dimensional tables, show how to implement a table in which values are
; stored under an arbitrary number of keys and different values may be stored under different numbers of keys. 
; The lookup and insert! procedures should take as input a list of keys used to access the table.

(define (make-table same-key?)
    (let ((table (list '*table*)))
        
        (define (print-table) (display table))
        
        (define (lookup . keys)
            (define (last? keys) (null? (cdr keys)))
            
            (define (parse-value table)
                (if (null? table)
                    '()
                    (let ((data (car table)))
                         (if (= (length data) 1)
                             (caar table)
                             (parse-value (cdr table))))))
            
            (define (find-value keys table)
                ; (display "[DBG] <find-value> keys : ") (display keys) (newline)
                ; (display "[DBG] <find-value> table : ") (display table) (newline)
                (define (matched-table key table)  
                    ;(display "[DBG] <matched-table> table : ") (display table) (newline)
                    (cond ((or (null? key) (null? table)) '())
                          ((equal? key (caar table)) (cons (cdar table) (matched-table key (cdr table))))
                          (else (matched-table key (cdr table)))))
            
                ; (if (not (null? keys))
                ;     (let ((result (matched-table (car keys) table)))
                ;           (display "[DBG] <find-value> result : ") (display result) (newline)))
                (cond ((null? table) #f)
                      ((null? keys) (parse-value table))
                      (else (find-value (cdr keys) (matched-table (car keys) table)))))
                      
            (let ((value (find-value keys (cdr table))))
                 (if value
                     value
                     (error "Unknown Key : Keys" keys))))
                 
        (define (assoc key records)
            (cond ((null? records) #f)
                  ((null? key) #t)
                  ((same-key? key (caar records)) (car records))
                  (else (assoc key (cdr records)))))
        
        (define (insert! . keys-value)
            (define (iter keys-value key table)
                (if (null? (cdr keys-value))
                  (let ((value (car keys-value)))
                       (let ((record (assoc key (cdr table))))
                            (if record
                                (set-cdr! record value)
                                (set-cdr! table (cons (cons (key value))
                                                      (cdr table))))))
                  (let ((key (car keys-value)))
                       (let ((key-exist? (assoc key (cdr table))))
                            (if key-exist?
                                (iter keys-value key (cdr table))
                                (let ((value (cdr keys-value)))
                                    (set-cdr! table (cons (cons key value)
                                                          (cdr table)))))))))
                  
            (iter keys-value (car keys-value) table))
          
         (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                  ((eq? m 'insert!) insert!)
                  ((eq? m 'print) (print-table))
                  (else (error "Unknown operaiotnl : TABLE" m))))
         dispatch))
     
     
(define t (make-table (lambda (x y) (equal? x y))))
((t 'insert!) 'a 'b 'c 1)
((t 'insert!) 'a 'b 2)
((t 'insert!) 'c 3)
((t 'insert!) 'd 4)
((t 'insert!) 'e 5)
((t 'insert!) 'f 6)

(t 'print)

(newline)
((t 'lookup) 'f)
((t 'lookup) 'e)
((t 'lookup) 'd)
((t 'lookup) 'c)
((t 'lookup) 'a 'b)
((t 'lookup) 'a 'b 'c)
((t 'lookup) 'a 'ff)