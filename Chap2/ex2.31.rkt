#lang sicp

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))
        (else (proc tree))))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(define tree (list 1 2 (list 3 (list 4 5))))
(square-tree tree)