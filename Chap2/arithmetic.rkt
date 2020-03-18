#lang sicp

(define (make-interval x y)
  (cons x y))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (width x)
  (/ (abs (- (upper-bound x) (lower-bound x))) 2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
"""
x [+,+]이고, y가 [-,-] 인경우 X
x [+,+]이고, y가 [-,+] 인경우 X
x [+,+]이고, y가 [+,+] 인경우 X
x [-,+]이고, y가 [-,-] 인경우 X 역으로 주면 됨
x [-,+]이고, y가 [-,+] 인경우 O 
x [-,+]이고, y가 [+,+] 인경우 X
x [-,-]이고, y가 [-,-] 인경우 X
x [-,-]이고, y가 [-,+] 인경우 X 역으로 주면 됨
x [-,+]이고, y가 [+,+] 인경우 X
"""
(define (mul-interval-update x y)
  (define (neg? n)
    (< n 0))
  (define (pos? n)
    (> n 0))
    (let ((l_x (lower-bound x))
        (u_x (upper-bound x))
        (l_y (lower-bound y))
        (u_y (upper-bound y)))
          ; -, + / - -     
    (cond ((and (neg? l_x) (pos? u_x) (neg? l_y) (neg? u_y)) (make-interval (* u_x l_y) (* l_x l_y)))

          ; -, - / - +
          ((and (neg? l_x) (neg? u_x) (neg? l_y) (pos? u_y)) (make-interval (* l_x u_y) (* l_x l_y)))

          ; -,+ / -,+
          ((and (neg? l_x) (pos? u_x) (neg? l_y) (pos? u_y)) (make-interval (min (* l_x u_y) (* u_x l_y)) (max (* l_x l_y) (* u_x u_y))))
                                                                            
          (else (mul-interval x y)))))
                                                             
           
          
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((upper (upper-bound x))
        (lower (lower-bound x)))
    (if (<= 0 (* upper lower))
        (error "Divison Error : Can not divide with zero")
        (mul-interval x
                (make-interval (/ 1. upper)
                               (/ 1. lower))))))
             
(define (print-interval x)
    (display "(")
    (display (lower-bound x))
    (display " ~ ")
    (display (upper-bound x))
    (display ")")
    (newline))

(display "T1 : ")
(define t1 (make-interval -10 40))
(print-interval t1)
(display (width t1)) (newline)

(display "T2 : ")
(define t2 (make-interval -15 30))
(print-interval t2)
(display (width t2)) (newline)

(display "ADD_RESULT @@ \n")
(define add-result (add-interval t1 t2))
(print-interval add-result)
(display (width add-result)) (newline)

(display "MULT_RESULT @@ \n")
(define mult-result (mul-interval t1 t2))
(print-interval mult-result)
(display (width mult-result)) (newline)

(display "DIV_RESUlT @@ \n")
(define div-result (div-interval t1 t2))
(print-interval div-result)
(display (width div-result)) (newline)

(define (eq? old new)
  (let ((old-low (lower-bound old))
        (old-up (upper-bound old))
        (new-low (lower-bound new))
        (new-up (upper-bound new)))
    (if (and (= old-low new-low) (= old-up new-up))
        (display "TEST PASSED\n")
        (begin
          (display "OLD :")
          (print-interval old)
          (display "NEW :")
          (print-interval new)
          (error "<<< Value is Different\n")))))

(display "STRESS TEST @@@ \n")
(let ((t_1 (make-interval 10 25))
      (t_2 (make-interval 20 40)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval 10 25))
      (t_2 (make-interval -20 40)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval 10 25))
      (t_2 (make-interval -40 -20)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval -10 25))
      (t_2 (make-interval 20 40)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval -10 25))
      (t_2 (make-interval -20 40)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval -10 25))
      (t_2 (make-interval -40 -20)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval -25 -10))
      (t_2 (make-interval 20 40)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval -25 -10))
      (t_2 (make-interval -20 40)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))
(let ((t_1 (make-interval -25 -10))
      (t_2 (make-interval -40 -20)))
  (eq? (mul-interval t_1 t_2) (mul-interval-update t_1 t_2)))