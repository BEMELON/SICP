#lang sicp
(#%provide smallest-divisor)
(#%provide prime?)
(#%provide fast-find-divisor)
(#%provide fast-prime?)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (fast-smallest-divisor n)
  (fast-find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (next divisor)
  (if (= divisor 2)
      (+ 1 divisor)
      (+ 2 divisor)))
     
;; ex1.23
(define (fast-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))


(define (fast-prime? n)
  (= n (fast-smallest-divisor n)))

(define (prime? n)
  (= n (smallest-divisor n)))
