#lang sicp

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (* 2 tree))))

(define (square-tree-1 tree)
  (map square-tree tree))

(define tree (list 1 (list 2 3) (list 2 3 4 5)))
(square-tree tree)
(square-tree-1 tree)