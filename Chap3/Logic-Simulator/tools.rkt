#lang sicp
(#%provide get-signal set-signal add-action! make-wire)

; Primitive function boxes
(define (get-signal wire)
    (wire 'status))

(define (set-signal! wire value)
    (wire 'set-current! value))

(define (add-action! wire action)
    (wire 'set-action! action))
    
(define (make-wire)
    (let ((status 0)
          (action '()))
      
      (define (set-current! current)
          (set! status current)
          (action))
      
      (define (set!-action! proc)
          (set! action proc))
      
      (define status (display status) (newline))
      
      (define (dispatch m)
          (cond ((equal? m 'status) status)
                ((equal? m 'set-current!) set-current!)
                ((equal? m 'set-action!) set-action!)
                (else (error "WIRE : No matching Messages : " m))))
      dispatch))



          
          
         