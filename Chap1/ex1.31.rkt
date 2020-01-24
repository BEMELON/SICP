#lang sicp

(define (next x) (+ x 2))

(define (pi-product a next times)
  (define first (/ (- a 1) a))
  (define second (/ (+ a 1) a))
  (cond ((= times 0) 1)
        (else (* first second (pi-product (next a) next (- times 1))))))

(define (product a next times result)
  (define first (/ (- a 1) a))
  (define second (/ (+ a 1) a))
  (cond ((= times 0) result)
        (else (product (next a) next (- times 1) (* result first second)))))

(* 4 (pi-product 3.0 next 1000))
(* 4 (product 3.0 next 1000 1))
  