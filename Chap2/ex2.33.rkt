#lang sicp
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
            (accumulate op init (cdr seq)))))

;#1
(define (square x) (* x x))
(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))

              
(map (lambda (x) (* 2 x)) (list 1 2 3 4))

;#2
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

;#3
(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(length (list 1 2 3 4 5))

