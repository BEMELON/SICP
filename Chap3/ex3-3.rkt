#lang simply-scheme

; Exercise 3.3
; Modify the make-account procedure so that it creates password-protected accounts.
; That is, make-account should take a symbol as an additional argument, as in
; (define acc (make-account 100 'secret-password))

; The resulting account object should process a request only
; if it is accompanied by the password with which the account was created, and should otherwise return a complaint

; ((acc 'secret-password 'withdraw) 40)
; 60

; ((acc 'some-other-password 'deposit) 50)
; "Incorrect password"

(define (make-account balance a-password)
    (define (check-password i-password) (equal? i-password a-password))
    
    (define (withdraw amount)
        (if (> amount balance)
            '(Insufficient funds)
            (begin (set! balance (- balance amount)) balance)))
            
    (define (deposit amount)
       (begin (set! balance (+ balance amount)) balance))

    (define (dispatch i-password m)
        (if (check-password i-password)
            (cond ((equal? m 'withdraw) withdraw)
                  ((equal? m 'deposit) deposit)
                  (else "Incorrect Message"))
            (lambda (.whatever) "Incorrect password")))
    dispatch)

; (define acc (make-account 100 'secret-password))
; ((acc 'secret-password 'withdraw) 40)
; ((acc 'secret-password 'withdraw) 140)
; ((acc 'some-other-password 'deposit) 50)