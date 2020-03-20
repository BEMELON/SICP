#lang sicp
(#%require "arithmetic.rkt")

; [Val-center, Val-tolerence]  => [Val-center, Percent-tolerence]
(define (make-with-width c t)
  (let ((percent_width (* (/ t c) 100.)))
    (make-interval c percent_width)))

(define (percent i)
  (cdr i))

(define (width i)
  (* (* (/ (percent i) 100.) (center i)) 100))

(define (center i)
  (car i))


; ex1.3 
; a = 100 / 5 ; a = [100, 5]
; b = 30 / 1.5 ; b = [30, 1.5]
; c = a * b = 3000 / c = [95 * 27 , 105 * 33] = [3015, 15]
; c = [(a + (a * 0.05)) * (b + (b * 0.1)),
;     [(a - (a * 0.05)) * (b - (b * 0.1)))
; c = [(a + Ta) * (b + Tb),
;     [(a - Ta) * (b - Tb)]
; c = [ab + aTb + bTa + TaTb,
;     [ab - aTb - bTa + TaTb];
; So, c = [ab + TaTb, 0.0025]

(define (mult-interval a b)
  (let ((Ta (* (center a) (/ (percent a) 100))) ; expect 5
        (Tb (* (center b) (/ (percent b) 100))) ; expect 1.5
        (Va (center a))
        (Vb (center b)))
    (make-interval (+ (* Va Vb) (* Ta Tb))
                   (* (/ (percent a) 100.) (/ (percent b) 100.)))))

(define t1 (make-with-width 100 5))
(define t2 (make-with-width 30 1.5))
(print-interval t1)
(print-interval t2)
(define t3 (mult-interval t1 t2))
(print-interval t3)

