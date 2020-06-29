#lang sicp
(#%provide leaf? left-branch right-branch
        symbol-leaf weight-leaf symbol symbols 
        weight make-leaf make-code-tree adjoin-set)

(define (leaf? tree) (eq? (car tree) 'leaf))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbol-leaf branch) (cadr branch))
(define (weight-leaf branch) (caddr branch))
(define (symbol branch) (cadr branch))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (make-code-tree left right) (list left right 
                                          (append (symbols left) (symbols right)) 
                                          (+ (weight left) (weight right))))
                                      
(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))