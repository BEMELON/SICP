#lang sicp
(define (square x)
  (* x x))

(define (sumOfsquare x y z)
  (+ (square x) (square y) (square z)))

(define (getMin x y)
  (if (> x y) y
      x))

(define (ex1.3 a b c)
  (define sum (sumOfsquare a b c))
  (define min (getMin (getMin a b) c))
  (- sum (square min)))


(ex1.3 1 2 3)
(ex1.3 4 5 6)
   
      
      
 