#lang sicp
(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (reverse li)
  (define (reverse-travel li tmp)
    (if (null? li)
        tmp
        (reverse-travel (cdr li) (cons (car li) tmp))))
  (reverse-travel li nil))

(reverse (list 1 2 3 4 5))