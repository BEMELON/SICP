#lang sicp

(define (make-leaf symbol frequency) (list 'leaf symbol frequency))
(define (weight x) (caddr x))
(define (symbol x) (cadr x))
(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))
                  
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
             (adjoin-set (make-leaf (car pair) ;symbol
                                     (cadr pair)) ;frequency
                         (make-leaf-set (cdr pairs))))))


(define (successive-merge sets)
    (define (merge x y)
        (make-leaf 
                (list (symbol x) (symbol y))
                (+ (weight x) (weight y))))
    (if (= (length sets) 1)
        sets
        (successive-merge (adjoin-set 
                                (merge (car sets) (cadr sets)) 
                                (cddr sets)))))
    

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
