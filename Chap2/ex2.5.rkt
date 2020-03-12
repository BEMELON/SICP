#lang sicp
(define (cons a b)
    (define (compute base exp)
        (if (= exp 0)
            1
            (* base (compute base (- exp 1)))))

    (define (dispatch m)
        (if (= m 0)
            (compute 2 a)
            (compute 3 b)))
    dispatch)

(define (car m)
    (m 0))

(define (cdr m)
    (m 1))

(define t (cons 2 3))
(display (car t))
(newline)
(display (cdr t))