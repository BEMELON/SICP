#lang sicp
  
     
(define (equal? x y) 
    ;(display "[DBG] <equal?> x |=> ") (display x) (newline)
    ;(display "[DBG] <equal?> y |=> ") (display y) (newline)
    (if (and (null? x) (null? y)) 
        #true
          (begin
              (let ((token_x (car x))
                    (token_y (car y)))
                
                  (cond   ((and (symbol? token_x) (symbol? token_y)) 
                                (and (eq? token_x token_y)
                                     (equal? (cdr x) (cdr y))))                                                          
                          ((and (not (pair? token_x)) (pair? token_y))  
                                (and (equal? (list token_x) (list (car token_y)))
                                     (equal? (list (cadr x)) (list (cadr token_y)))
                                     (equal? (list (cdr (cdr x))) (list (cdr y)))))
                                     
                          ((and (pair? token_x) (not (pair? token_y)))
                                (and (equal? (list (car token_x)) (list token_y))
                                     (equal? (list (cadr token_x)) (cdr y))
                                     (equal? (list (cdr x)) (list (cdr (cdr y))))))
                          (else 
                                (and (equal? (list (car token_x)) (list (car token_y))))))))))



(equal? '(this is a list) '(this is a list))      
(equal? '(this is a list) '(this (is a) liste))