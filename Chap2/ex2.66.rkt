#lang sicp

(define (entry tree) (car tree))
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))

(define (lookup given-key tree-of-records)
    (cond ((null? tree-of-records) #f)
          ((> given-key (key (entry tree-of-records))) (lookup given-key (right-tree tree-of-records)))
          ((> (key (entry (tree-of-records))) given-key) (lookup given-key (left-tree tree-of-records)))
          (else (entry tree-of-records))))