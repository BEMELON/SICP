#lang sicp

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (entry tree) (car tree))


; b. Do the two procedures have the same order of growth
; in the number of steps required to convert a balanced
; tree with n elements to a list? If not, which one grows
; more slowly?

(define fig2-17 (list 1 nil (list 2 nil (list 3 nil (list 4 nil (list 5 nil (list 6 nil (list 7 nil nil))))))))


(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree) 
                          (cons (entry tree)
                                (copy-to-list (right-branch tree) 
                                              result-list)))))
    (copy-to-list tree '()))


(define (tree->list-1 tree)
    (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree) (tree->list-1 (right-branch tree))))))
        
(tree->list-1 fig2-17)
(tree->list-2 fig2-17)