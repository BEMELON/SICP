#lang sicp
(#%require "huffman.rkt")
(#%provide generate-huffman-tree)
                 
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
             (adjoin-set (make-leaf (car pair) ;symbol
                                     (cadr pair)) ;frequency
                         (make-leaf-set (cdr pairs))))))

 (define (successive-merge leaves) 
   (if (null? (cdr leaves)) 
       (car leaves) 
       (successive-merge 
        (adjoin-set (make-code-tree (car leaves) (cadr leaves)) 
                    (cddr leaves)))))  
    

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

;(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
