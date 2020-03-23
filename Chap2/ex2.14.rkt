#lang sicp
(#%require "ex2.12-13.rkt")
(#%require "arithmetic.rkt")

; R1*R2 / (R1 + R2) => this wrong why?
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

; ( 1 / (1 / R1) + (1 / R2) => this corrects
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(define t1 (make-with-width 5 3))
(define t2 (make-with-width 5 3))
(display "T1 : ") (print-interval t1)
(display "T2 : ") (print-interval t2)
(define r1 (par1 t1 t2)) ; 20000 / 300 -> 66.6666 / 
(define r2 (par2 t1 t2))
(print-interval r1)
(print-interval r2) 
(display "@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
(define r3 (par1 t1 t1))
(define r4 (par2 t1 t1))
(print-interval r3)
(print-interval r4)