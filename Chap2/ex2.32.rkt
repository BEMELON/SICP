#lang sicp
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x) (cons (car s) x))
                       rest)))))

; 프로시저에서 재귀를 쌓을 때, 첫번째 원소를 제외하고 다시 재귀를 넣기 때문에
; 이를 합치는 과정에서는 첫번째 원소와 return 받은 값(첫번째 원소를 제외한 부분집합)를
; 서로 append 를 하면 해결이 된다.
(subsets (list 1 2 3))

