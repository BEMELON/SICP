#lang sicp
(define (filtered-accumulate predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a) (filtered-accumulate predicate combiner null-value term (next a) next b)))
        (else (filtered-accumulate predicate combiner null-value term (next a) next b))))

; (A)
(define (divide? n x)
  (= (remainder x n) 0))

(define (prime? x)
  (define (check-it n)
    (cond ((> n (- x 1)) true)
          ((divide? n x) false)
          (else (check-it (+ 1 n)))))
  (check-it 2))

(filtered-accumulate prime?
                    (lambda (x y) (+ x y))
                    0
                    (lambda (x) (* x x))
                    2
                    (lambda (x) (+ x 1))
                    10)


; (B)
; (filtered-accumulate predicate combiner null-value term a next b)
(define (GCD x y)
  (if (= y 0)
      x
      (GCD y (remainder x y))))

(define (coprime-product N)
  (define (coprime? k)
    (= 1 (GCD N k)))
  (filtered-accumulate coprime?
                       (lambda (x y) (* x y))
                       1
                       (lambda (x) x)
                       2
                       (lambda (x) (+ 1 x))
                       N))

(coprime-product 10)
                     