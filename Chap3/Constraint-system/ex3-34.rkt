#lang sicp

; Exercise 3.34
; Louis Reasoner wants to build a squarer, 
; a constraint device with two terminals such 
; that the value of connector b on the second terminal will always be the square of the value a on the first terminal.
; He proposes the following simple device made from a multiplier:
;     (define (squarer a b)
;         (multiplier a a b))
; There is a serious flaw in this idea. Explain.

; case a) (set-value! a 2 'a) will works
; case b) (set-value! b 4 'b) will not works, because the procedure works only if two terminals has value.
