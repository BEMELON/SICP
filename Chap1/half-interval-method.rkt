#lang sicp
(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value) (search f neg-point mid-point))
                ((negative? test-value) (search f mid-point pos-point))
                (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (positive? b-value) (negative? a-value))
           (search f a b))
          (else
           (error "Values are not of opposite sign" a b)))))

;(half-interval-method sin 2.0 4.0)
;(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
                                                         