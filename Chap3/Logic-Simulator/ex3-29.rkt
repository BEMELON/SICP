#lang sicp
(#%require "Logics.rkt")

; Exercise 3.29
; Another way to construct an or-gate is as a compound digital logic device, built from and-gates and inverters. 
; Define a procedure or-gate that accomplish this. 
; What is the delay time of the or-gate in terms of andgate-delay and inverter-delay?

(define (or-gate a1 a2 output)
    (let ((w1 (make-wire))
          (w2 (make-wire))
          (w3 (make-wire)))
      (inverter a1 w1)
      (inverter a2 w2)
      (and-gate w1 w2 w3)
      (inverter w3 output)))
  
  