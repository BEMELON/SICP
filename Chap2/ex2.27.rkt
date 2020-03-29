#lang sicp
(define (reverse li)
  (define (reverse-travel li li-remain)
    (if (null? li)
        li-remain
        (reverse-travel (cdr li) (cons (car li) li-remain))))
  (reverse-travel li nil))

(define (deep-reverse li)
  (define (reverse-travel li li-remain)
    (cond ((null? li) li-remain)
          ((pair? (car li)) (reverse-travel (cdr li) (cons (reverse (car li)) li-remain)))
          (else (reverse-travel (cdr li) (cons (car li) li-remain)))))
  (reverse-travel li nil))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse (list 10 9 (list 2 3)))
(deep-reverse (list 4 3 2 1))
(deep-reverse x)