#lang sicp

;; ex2.29 -- (a)
; Mobile := <Branch, Branch>
(define (make-mobile left right)
  (list left right))

; Branch := <Int, Int/Branch>
; @Param
;  (car Branch) : length
;  (cdr Branch) : weight or structure
(define (make-branch length structure)
  (list length structure))

(define (branch mobile)  
  (define (append a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b))))
  (if (pair? (cdr mobile))
      (append (car mobile) (branch (cdr mobile)))
      nil))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-length branch)
  (car branch))

;; ex2.29 - (b) 
(define (total-weight mobile)
  (define (weight branch)
    (if (pair? (branch-structure branch))
        (* (branch-length branch) (weight (branch-structure branch)))
        (* (branch-length branch) (branch-structure branch))))
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

;; ex2.29 - (c)
(define (balance? mobile)
  (define (test left right)
    (define (same? branch left_length right_length)
      (let ((left_weight (car branch))
            (right_weight (cdr branch)))
        (list branch
              (= (* left_weight left_length)
                 (* right_weight right_length)))))
      (cond ((and (pair? left) (pair? right)) (same? (test (branch-structure left) (branch-structure right))
                                                     (branch-length left)
                                                     (branch-length right)))
            ((and (pair? left) (not (pair? right))) (same? (test (branch-structure left) right)
                                                     (branch-length left)
                                                     (branch-length right)))
            ((and (not (pair? left)) (pair? right)) (same? (test left (branch-structure right))
                                                     (branch-length left)
                                                     (branch-length right)))
            (else (list left right))))
          
  (test (left-branch mobile)
        (right-branch mobile)))

;; 취약 케이스
(define left_a (make-branch 6 3))
(define left_b (make-branch 3 left_a))
(define right_a (make-branch 3 6))
(define right_b (make-branch 3 right_a))
(define mob (make-mobile left_b right_b))
(total-weight mob)
(balance? mob)