#lang sicp
(#%require "smallest-divisor.rkt" "../util/util.rkt")

(define (prime? n)
  (= n (smallest-divisor n (lambda (divisor) (+ 1 divisor)))))

(define (next divisor)
  (if (= divisor 2)
      (+ 1 divisor)
      (+ 2 divisor)))

(define (fast-prime? n)
  (= n (smallest-divisor n next)))

(search-for-primes 100000000000 100000000060  prime?)
(search-for-primes 100000000000 100000000060  fast-prime?)