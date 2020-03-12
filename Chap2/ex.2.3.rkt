#lang sicp
(#%require "Point.rkt")

(define (make-rect seg_horizon seg_vertical)
  (cons seg_horizon seg_vertical))

(define (horizon_Segment rect)
  (car rect))

(define (vertical_Segment rect)
  (cdr rect))

(define (left-Point seg)
  (start-segment seg))

(define (right-Point seg)
  (end-segment seg))

; Get Distance of segment
(define (distance segment)
  (define (compute p1 p2)
    (sqrt (+ (square (- (x-point p1)
                        (x-point p2)))
             (square (- (y-point p1)
                        (y-point p2))))))
  (define (square x) (* x x))
  (let ((p1 (left-Point segment))
        (p2 (right-Point segment)))
    (compute p1 p2)))
        

; Get width, If width is Zero then return width instead
; => Can't determine which is height or width
(define (width seg_horizon)
  (let ((_width (abs (- (x-point (left-Point seg_horizon))
                        (x-point (right-Point seg_horizon))))))
    (if (= _width 0)
        (height seg_horizon)
        _width)))

; Get Height, If height is Zero then return Width instead.
; => Can't determine which is height or width
(define (height seg_vertical)
  (let ((_height (abs (- (y-point (left-Point seg_vertical))
                        (y-point (right-Point seg_vertical))))))
    (if (= _height 0)
        (width seg_vertical)
        _height)))

(define (rect-area rect)
  (* (width (horizon_Segment rect))
     (height (vertical_Segment rect))))

(define (rect-perimeter rect)
  (* (+ (distance (horizon_Segment rect))
        (distance (vertical_Segment rect)))
     2))

; Test code
; expect :
;  - area : 100
;  - circum : 48 ~ 
(define s_vertical (make-segment (make-point 10 15)
                               (make-point  20 25)))
(define s_horizon (make-segment (make-point 20 15)
                              (make-point 10 15)))

(define rect_org (make-rect s_vertical s_horizon))

(display "TEST Value\n")
(display (rect-area rect_org))
(newline)
(display (rect-perimeter rect_org))
                           