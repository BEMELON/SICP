#lang sicp


(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf branch) (cadr branch))
(define (weight-leaf branch) (caddr branch))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (make-code-tree left right) (list left right 
                                          (append (symbols left) (symbols right)) 
                                          (+ (weight left) (weight right))))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit: CHOOSE_BRANCH" bit))))
                                        
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                 (if (leaf? next-branch)
                     (cons (symbol-leaf next-branch)
                           (decode-1 (cdr bits) tree))
                     (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-tree 
            (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                            (make-code-tree (make-leaf 'D 1)
                                            (make-leaf 'C 1)))))

(display sample-tree) (newline)                                                        
(decode sample-message sample-tree)