#lang sicp 
(#%require "op.rkt")
(#%provide get-signals get-signal set-signal! add-action!
           or-gate and-gate inverter make-wire
           half-adder full-adder)
       
(define (get-signal wire)
    (wire 'get-current!))

(define (get-signals S)
    (if (null? S)
        (display "")
        (begin (get-signal (car S))
               (get-signals (cdr S)))))
           
(define (set-signal! wire value)
    ((wire 'set-current!) value))

(define (add-action! wire action)
    ((wire 'add-action!) action))

(define (after-delay delay proc)
    (delay)
    (proc))


(define (make-wire)
    (let ((status -1)
          (action-procedure '()))

      (define (call-each procedures) 
          (if (null? procedures)
              '(call-each ==> done)
              (begin ((car procedures))
                      (call-each (cdr procedures)))))
                
      (define (set-current! current)
          (if (not (= current status))
            (begin (set! status current)
                   (call-each action-procedure)))
           '(set-current! ==> done))
     
      (define (add-action! proc)
          (set! action-procedure (cons proc action-procedure)))
      
      (define (print-status) 
          (display "current ==> ") (display status) (newline)
          (display "action-procedure ==> ") (display action-procedure) (newline))
      
      (define (dispatch m)
          (cond ((equal? m 'status) (print-status))
                ((equal? m 'get-current!) status)
                ((equal? m 'set-current!) set-current!)
                ((equal? m 'add-action!) add-action!)
                (else (error "WIRE : No matching Messages : " m))))
      dispatch))

(define (or-gate a1 a2 output)
    (define (logical-or a1 a2)
        (if (or (= a1 1) (= a2 1))
            1
            0))
        
    (define (or-action-procedure)
        (let ((computed (logical-or (get-signal a1) (get-signal a2))))
             ;(display output) (newline) (display computed) (newline)
             (after-delay
                 ;or-gate-delay
                 (lambda () (display ""))
                 (lambda () (set-signal! output computed)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    '(or-gate ==> ok))

(define (and-gate a1 a2 output)
    (define (logical-and a1 a2)
        (if (and (= a1 1) (= a2 1))
            1
            0))
    (define (and-action-procedure)
        (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
            (after-delay
                ;and-gate-delay
                (lambda () (display ""))
                (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    '(and-gate ==> ok))

(define (inverter input output)
    (define (logical-not s)
        (cond ((= s 0) 1)
              ((= s 1) 0)
              (else (error "Invalid signal" s))))
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay 
                ;inverter-delay
                (lambda () (display ""))
                (lambda () (set-signal! output new-value)))))
    (add-action! input invert-input) 
    '(inverter ==> ok))

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
    '(half-adder ==> ok)))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
    '(full-adder ==> ok)))