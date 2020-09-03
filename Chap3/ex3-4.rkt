#lang simply-scheme

; Exercise 3.4 
; Modify the make-account procedure of Exercise 3.3 by adding another local state variable so that, 
; if an account is accessed more than seven consecutive times with an incorrect password, 
; it invokes the procedure call-the-cops.

(define (make-account balance a-password)
    (define (call-the-cops) "Calling cops...")
    
    (define (check-password i-password) (equal? i-password a-password))
    
    (define (withdraw amount)
        (if (> amount balance)
            '(Insufficient funds)
            (begin (set! balance (- balance amount)) balance)))
            
    (define (deposit amount)
        (begin (set! balance (+ balance amount)) balance))

    (define (dispatch m)
        (cond ((equal? m 'withdraw) withdraw)
              ((equal? m 'deposit) deposit)
              (else "Incorrect Message")))
        
    (let ((count-failed-password 0))
        (lambda (i-password m)
            (if (check-password i-password)
                (dispatch m)
                (begin
                    (set! count-failed-password (+ count-failed-password 1))
                    (if (> count-failed-password 7)
                        (lambda (.whatever) (call-the-cops))
                        (lambda (.whatever) "Incorrect password")))))))
                      
            
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'withdraw) 140)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)