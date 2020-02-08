#lang sicp

; print-point
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
            
; make-point
(define (make-point x y)
  (cons x y))

; x-point
(define (x-point point)
  (car point))
  
; y-point
(define (y-point point)
  (cdr point))

; make-segment
(define (make-segment p1 p2)
  (cons p1 p2))

; start-segment
(define (start-segment p)
  (car p))

; end-segment
(define (end-segment p)
  (cdr p))

; midpoint-segment
(define (midpoint-segment s1 s2)
  (define (average x y) (/ 2 (+ x y)))
  (make-point (average (start-segment s1) (start-segment s2))
              (average (end-segment s1) (end-segment s2))))

