#lang sicp
;(accumulate combiner null-value term a next b)
; combiner : 이어지는 두 값을 묶는 프로시저
; null-value : 계산할 값이 없을 때 사용하는 인자
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner result term a next b)
  (if (> a b)
      result
      (accumulate combiner (combiner result (term a)) term (next a) next b)))

;; ex1.30
(accumulate (lambda (x y) (+ x y))
            0
            (lambda (x) (* x x))
            0
            (lambda (x) (+ 1 x))
            10)

;; ex1.31
(* 4
 (accumulate (lambda (x y) (* x y))
           1
           (lambda (x) (* (/ (- x 1) x) (/ (+ x 1) x)))
           3.0
           (lambda (x) (+ x 2))
           1000))
