#lang sicp
(#%require "arithmetic.rkt")
(#%provide make-with-width percent center width)

; [Val-center, Per-tolerence]  => [Val-center, Val-tolerence]
(define (make-with-width c t)
  (let ((value-tolerence (* c (/ t 100.))))
    (make-interval (- c t) (+ c t))))

(define (percent i)
    (* (/ (width i) (center i)) 100.))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;(define t1 (make-with-width 100 5))
;(display (percent t1)) (newline)
;(display (width t1)) (newline)
;(display (center t1)) (newline)

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
; So, c = [ab + TaTb, 0.05 * 0.05]
