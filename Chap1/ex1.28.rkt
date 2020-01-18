#lang sicp
; Miller-Rabin Primality Test
;(#%require "smallest-divisor.rkt" "../util/util.rkt")
(#%require (lib "27.ss" "srfi")) ;; To use random-integer 


(define (square x) (* x x))

(define (check-trival x square N)
  (if (and (= square 1)
           (not (= x 1))
           (not (= x (- N 1))))
      0
      square))

(define (non-trival? x N)
  (check-trival x (remainder (square x) N) N))

(define (expmod base exp N)
  (cond ((= exp 0) 1)
        ((even? exp) (non-trival? (remainder (square (expmod base (/ exp 2) N)) N) N))
        (else (remainder (* base (expmod base (- exp 1) N)) N))))


; True if true for "times" else false
; N => N-1
(define (check-it N_ times)
  (define N (+ N_ 1))
  (define A (+ (random N_) 1))
  (cond ((= times 0) true)
        ((= (expmod A N_ N) 0) false)
        (else (check-it N_ (- times 1)))))

; returns : True if Prime else False
(define (prime? N times)
  (define N_ (- N 1)) 
  (check-it N_ times))

(define (search-for-primes N end times)
  (cond ((> N end) (display "DONE\n"))
        ((even? N) (search-for-primes (+ 1 N) end times))
        ((prime? N times) (begin
                      (report-prime N)
                      (search-for-primes (+ 2 N) end times)))
        (else (search-for-primes (+ 2 N) end times))))

(define (report-prime N)
  (display N)
  (display " IS A PRIME!")
  (newline))


; 177 161 141 133 129 has error
(search-for-primes 100 200 100)