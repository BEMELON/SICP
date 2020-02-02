#lang sicp
(define (cont-frac-iter n d k result)
  (if (= k 0)
      result
      (cont-frac-iter n d (- k 1) (/ (n k) (+ result (d k))))))

(define (euler-k k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i)
                    (let ((left (remainder i 3)))
                      (cond ((= left 0) 1.0)
                            ((= left 1) 1.0)
                            ((= left 2) (* 2 (/ (+ i 1) 3))))))
                  k
                  1)))

(euler-k 10)
(euler-k 100)
(euler-k 1000)
(euler-k 10000)
