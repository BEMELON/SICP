#lang sicp

(define (root-cube x)
  (define (abs x)
    (if (> x 0) x
        (* -1 x)))
  
  (define (cube-root guess x)
    (if (good-enough? guess x)
        guess
        (cube-root (improve guess x)
                   x)))

  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  
  (define (print data)
    (display "=====\n=>")
    (display data)
    (display "<=\n===="))
  
  (define (improve guess x)
    (average (+ (/ x (square guess)) (* 2 guess))))

  (define (average x)
    (/ x 3))

  (define (cube x) (* x x x))
  (define (square x) (* x x))

  (cube-root 1.0 x)
  )

(root-cube 27); 3.0000005410641766
(root-cube 64); 4.000017449510739
(root-cube 127); 5.026525695751077