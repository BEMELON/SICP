#lang sicp
(#%require "ex1.43.rkt")
(define tolerence 0.0001)

(define (print-data data)
  (display "**** ")
  (display data)
  (display " ****\n"))

(define (fixed-point f guess)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerence))
  
  (define (try guess)
    (let ((next (f guess)))
      ;(print-data next)
      ;(print-data guess)
      (if (close-enough? next guess)
          next
          (try next))))
  
  (try guess))


(define (n-root x n)

  (define (n-pow x n)
    (if (= n 0)
        1
        (* x (n-pow x (- n 1)))))

  (define (average x y) (/ (+ x y) 2))
  
  (define (average-damp f)
    (lambda (x) (average x (f x))))

  (fixed-point ((repeated average-damp n) (lambda (y) (/ x (n-pow y (- n 1))))) 1.0))

;(n-root 81 4)