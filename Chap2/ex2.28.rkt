#lang sicp

(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else tree)))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2 3) (list 4 5 6)))
(fringe x)
(fringe (list x y))
(fringe y)
