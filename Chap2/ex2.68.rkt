#lang sicp



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

(define sample-tree 
            (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                            (make-code-tree (make-leaf 'D 1)
                                            (make-leaf 'C 1)))))
(define sample-message '(A D A B B C A))


(define (encode-symbol word tree)
    (define (exist? word symbols)
        (cond ((null? symbols) #f)
              ((eq? word (car symbols)) #t)
              (else (exist? word (cdr symbols)))))
    
    (define (encode-symbol-1 word tree)
        
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
          
    (if (exist? word (symbols tree))
        (encode-symbol-1 word tree)
        (error "[ERROR] <encode-symbol> symbol doesn't exist in tree" word)))
    

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))
            
(encode sample-message sample-tree)