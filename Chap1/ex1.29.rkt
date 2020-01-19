#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))  
  (define (simpson-sum idx)
    (define y (f (+ a (* idx h))))
    (cond ((= idx 0) (+ y (simpson-sum (+ 1 idx))))
          ((= idx n) y)
          ((even? idx) (+ (* 2 y) (simpson-sum (+ 1 idx))))
          (else (+ (* 4 y) (simpson-sum (+ 1 idx))))))
    
  (* (simpson-sum 0) (/ h 3)))

(integral cube 0 1 0.001)
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)