#lang sicp
(define (for-each cmd li)
  (if (null? (cdr li))
      (cmd (car li))
      (begin
        (cmd (car li))
        (for-each cmd (cdr li)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))