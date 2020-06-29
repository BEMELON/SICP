#lang sicp
(#%require "huffman.rkt")
(#%provide encode)
(define sample-tree 
            (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                            (make-code-tree (make-leaf 'D 1)
                                            (make-leaf 'C 1)))))
(define sample-message '(A D A B B C A))


(define (encode-symbol word tree)
    (define (exist? word symbols)
        ;(display "[DBG] <exist?> word : ") (display word) (newline)
        (cond ((null? symbols) #f)
              ((eq? word (car symbols)) #t)
              (else (exist? word (cdr symbols)))))
    
    (define (encode-symbol-1 word tree)
       ; (display "[DBG] <encode-symbol-1> word : ") (display word) (newline)
        (define (encode-symbol-2 word tree code)
            (cond ((not (leaf? tree))
                       (append (encode-symbol-2 word (left-branch tree) (append code '(0)))
                              (encode-symbol-2 word (right-branch tree) (append code '(1)))))
                  ((not (eq? (symbol tree) word))
                       '())
                  (else code)))
              
        (if (null? word)
            '()
            (append (encode-symbol-2 word (left-branch tree) '(0))
                  (encode-symbol-2 word (right-branch tree) '(1)))))
    ;(display "[DBG] <encode-symbol> symbols : ") (display tree) (newline)      
    (if (exist? word (symbols tree))
        (encode-symbol-1 word tree)
        (error "[ERROR] <encode-symbol> symbol doesn't exist in tree" word)))
    

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))
            
;(encode sample-message sample-tree)