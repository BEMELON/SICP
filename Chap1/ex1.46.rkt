#lang sicp
(define tolerence 0.0001)
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))


(define (iterative-improve improve good-enough?)
  (lambda (x)
    (define (iter n)
       (if (good-enough? n)
           n
           (iter (improve n))))
     (iter x)))

;; **************************************************************************************


(define (sqrt x)
  ((iterative-improve (lambda (y) (average y (/ x y)))
                      (lambda (y) (< (abs (- (square y) x)) tolerence))) 1.0))

;; **************************************************************************************


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerence))
  ((iterative-improve f
                      (lambda (x) (close-enough? x (f x)))) first-guess))

;; **************************************************************************************


(sqrt 100)
(fixed-point cos 1.0)
   
     

  
    
  