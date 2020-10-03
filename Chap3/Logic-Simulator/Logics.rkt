#lang sicp 
(#%require "../Queue/Queue.rkt")
(#%provide get-signals get-signal set-signal! add-action!
           or-gate and-gate inverter make-wire
           half-adder full-adder
           make-agenda make-time-segment segments segment-time segment-queue empty-agenda? 
           current-time first-segment rest-segment set-current-time! set-segments! add-to-agenda! 
           remove-first-agenda-item! first-agenda-item)
      
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

(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time the-agenda))
                    action
                    the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        "[propagate] propagate ==> done"
        (let ((first-item (first-agenda-item the-agenda)))
             (first-item)
             (remove-first-agenda-item! the-agenda)
             (propagate))))

(define (probe name wire)
    (add-action! wire
                 (lambda () 
                     (newline)
                     (display name) (display " ")
                     (display (current-time the-agenda))
                     (display "  new-value = " )
                     (display (get-signal wire)))))

(define (make-agenda) (list 0)) ; { time : action-lists }

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-time-segment time queue) (cons time queue))

(define (segments agenda) (cdr agenda)) 
 
(define (segment-time s) (car s))
 
(define (segment-queue s) (cdr s))
         
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (current-time agenda) (car agenda))
 
(define (first-segment agenda) (car (segments agenda)))

(define (rest-segment agenda) (cdr (segments agenda)))

(define (set-current-time! agenda time) (set-car! agenda time))

(define (set-segments! agenda segments) (set-cdr! agenda segments))

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (car segments)))))
    
    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
             (insert-queue! q action)
             (make-time-segment time q)))
         
    (define (add-to-segments! segments)
        (if (= (segment-time (car segments)) time)
            (insert-queue! (segment-queue (car segments))
                           action)
            (let ((rest (cdr segments)))
                 (if (belongs-before? rest)
                     (set-cdr! segments (cons (make-new-time-segment time action)
                                              (cdr segments)))
                     (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
         (if (belongs-before? segments)
             (set-segments! agenda (cons (make-new-time-segment time action) segments))
             (add-to-segments! segments))))
         
(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
         (delete-queue! q)
         (if (empty-queue? q)
             (set-segments! agenda (rest-segment agenda)))))
         
(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty : FIRST-AGENDA-ITME")
        (let ((first-seg (first-segment agenda)))
             (set-current-time! agenda (segment-time first-seg))
             (front-queue (segment-queue first-seg)))))
                 

(define (make-wire)
    (let ((status -1)
          (action-procedure '()))

      (define (call-each procedures) 
          (if (null? procedures)
              "[make-wire] call-each ==> done"
              (begin ((car procedures))
                      (call-each (cdr procedures)))))
                
      (define (set-current! current)
          (if (not (= current status))
            (begin (set! status current)
                   (call-each action-procedure)))
            "[make-wire] set-current! ==> done")
     
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
                 or-gate-delay
                 (lambda () (set-signal! output computed)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    "[or-gate] or-gate ==> ok")

(define (and-gate a1 a2 output)
    (define (logical-and a1 a2)
        (if (and (= a1 1) (= a2 1))
            1
            0))
    (define (and-action-procedure)
        (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
            (after-delay
                and-gate-delay
                (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    "[and-gate] and-gate ==> ok")

(define (inverter input output)
    (define (logical-not s)
        (cond ((= s 0) 1)
              ((= s 1) 0)
              (else (error "Invalid signal" s))))
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay 
                inverter-delay
                (lambda () (set-signal! output new-value)))))
    (add-action! input invert-input) 
    "[inverter] inverter ==> ok)")

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
    "[half-adder] half-adder ==> ok"))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
    "[full-adder] full-adder ==> ok"))