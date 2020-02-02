#lang sicp
(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (display guess)
    (display "   Tries : ")
    (display count)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 0))

(define (average x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (/ (log 1000) (log x)))  2.0) ; 28 times
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0); 7 times