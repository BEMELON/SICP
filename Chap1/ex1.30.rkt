#lang sicp

;; recursive sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; iter sum
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum (lambda (x) (* x x)) 0 (lambda (x) (+ 1 x)) 10)
(iter-sum (lambda (x) (* x x)) 0 (lambda (x) (+ 1 x)) 10)