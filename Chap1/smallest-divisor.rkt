#lang sicp
(#%provide search-for-primes smallest-divisor
           square divides? start-prime-test)
; (define (smallest-divisor n term)
; (define (search-for-primes n limit  prime-method)

;; Common Module
(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor term)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (term test-divisor) term))))

(define (smallest-divisor n term)
    (find-divisor n 2 term))

;; Time Checking method
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; General method for Prime check
(define (search-for-primes n limit  prime-method)
  (cond ((> n limit) (display "\nDONE\n"))
        (else
         (begin (timed-prime-test n  prime-method)
                (search-for-primes (+ 1 n) limit prime-method)))))

(define (timed-prime-test n  prime-method)
  (start-prime-test n (runtime)  prime-method))

;; Display if N is Prime nubber
(define (start-prime-test n start-time prime-method)
      (if (prime-method n)
          (begin
            (newline)
            (display n)
            (report-prime (- (runtime) start-time)))))






