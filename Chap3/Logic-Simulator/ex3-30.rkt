#lang sicp
(#%require "Logics.rkt")
; Exercise 3.30
; Figure 3.27 shows a ripple-carry adder formed by stringing together n full-adders.
; This is the simplest form of parallel adder for adding two n-bit binary numbers.
; The inputs A1, A2, A3, . . ., An and B1, B2, B3, . . ., Bn are the two binary numbers to be added 
; (each Ak and Bk is a 0 or a 1). 
; The circuit generates S1, S2, S3, . . ., Sn, the n bits of the sum, and C, the carry from the addition.
; Write a procedure ripple-carry-adder that generates this circuit. 
; The procedure should take as arguments three lists of n wires each—the Ak , the Bk , and the Sk—and also another wire C.
; The major drawback of the ripple-carry adder is the need to wait for the carry signals to propagate.
; What is the delay needed to obtain the complete output from an n-bit ripple carry adder, 
; expressed in terms of the delays for and-gates, or-gates, and inverters?

(define (ripple-carry-adder Ak Bk Sk C)
    (define (iter-adder Ak Bk Sk c-in)
        (if (null? Ak)
            '(ripple-carry-adder ==> done)
            (let ((carry (make-wire)))
              (full-adder (car Ak) (car Bk) c-in (car Sk) carry)
              (iter-adder (cdr Ak) (cdr Bk) (cdr Sk) carry))))
    (iter-adder Ak Bk Sk C))

(define (build-wires wires)
    (if (null? wires)
        '()
        (let ((temp (make-wire))
              (value (car wires)))
             (set-signal! temp value)
             (cons temp (build-wires (cdr wires))))))
(define A (build-wires (list 0 0 0 1 0 1 0)))
(define B (build-wires (list 0 0 1 0 0 1 0)))
(define S (build-wires (list 0 0 0 0 0 0 0)))
(define C (make-wire))
(set-signal! C 0)

(ripple-carry-adder A B S C)
(propagate)

(get-signals S)
