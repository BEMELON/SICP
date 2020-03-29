#lang sicp
(define (same-parity . numbers)
  (let ((first-el (car (car numbers)))
        (numbers-li (car numbers)))
    (if (even? first-el)
        (parity-travel numbers-li 0)
        (parity-travel numbers-li 1))))

(define (parity-travel numbers-li remain)
  (let ((current-number (car numbers-li))
        (next-li (cdr numbers-li)))
    (cond ((null? next-li) (list current-number))
          ((= (remainder current-number 2) remain) (cons current-number (parity-travel next-li remain)))
          (else (parity-travel next-li remain)))))

(same-parity (list 1 2 3 4 5 6 7 8 9))
(same-parity (list 2 3 4 5 6 7 8 9 10))