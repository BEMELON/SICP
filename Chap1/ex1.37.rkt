#lang sicp

(define (cont-frac n d k)
  (if (= k 0)
      1
      (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))))

;; must be 0.61804...
; Greater than 10 
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)


(define (cont-frac-iter n d k result)
  (if (= k 0)
      result
      (cont-frac-iter n d (- k 1) (/ (n 1) (+ result (d 1))))))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           10
           1)