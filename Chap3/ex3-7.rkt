#lang simply-scheme

; Exercise 3.7 
; Consider the bank account objects created by make-account, with the password modification described in Exercise 3.3.
; Suppose that our banking system requires the ability to make joint accounts. 
; Define a procedure makejoint that accomplishes this. 
; make-joint should take three arguments. 
; The first is a password-protected account. 
; The second argument must match the password with which the account was defined in order for the make-joint operation to proceed.
; The third argument is a new password. 
; makejoint is to create an additional access to the original account using the new password.
; For example,  if peter-acc is a bank account with password open-sesame, 
; then will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. 
; You may wish to modify your solution to Exercise 3.3 to accommodate this new feature.
;
;(define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))

(define (make-joint org-acc org-password new-password)
    (begin ((org-acc org-password 'joint) new-password)
           org-acc))
    

(define (make-account balance a-password)
    (define (check-password i-password)
        (define (check-for-all test passwords)
            (cond ((null? passwords) #f)
                  ((not (pair? passwords)) (equal? test passwords))
                  ((equal? test (car passwords)) #t)
                  (else (and #t (check-for-all test (cdr passwords))))))
        (let ((passwords a-password))
             (check-for-all i-password passwords)))

    (define (joint new-password)
        (set! a-password (append (list a-password) (list new-password))))

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
                  ((equal? m 'joint) joint)
                  (else "Incorrect Message"))
            (lambda (.whatever) "Incorrect password")))
    dispatch)


; (define peter-acc (make-account 100 'open-sesame))
; ((peter-acc 'open-sesame 'withdraw) 40)
; ((peter-acc 'open-sesame 'withdraw) 140)

; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud)) 
; ((paul-acc 'rosebud 'withdraw) 10)
; ((paul-acc 'open-sesame 'withdraw) 10)
; ((paul-acc 'some-other-password 'deposit) 50)
