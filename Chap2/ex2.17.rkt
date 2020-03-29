#lang sicp
(define (last-pair li)
  (let ((next (cdr li)))
    (if (null? next)
        (car li)
        (last-pair next))))

(last-pair (list 23 72 149 34 25))
