#lang sicp
(#%provide add-rat sub-rat mult-rat div-rat equal-rat?
           print-rat make-rat)

(define (print-rat x)
  (display (numer x))
  (display " / " )
  (display (denom x))
  (newline))



; (cons x y) 1, 2 인 Pair 생성 (Construct)
; (car x) x의 첫번째 값 (Content of Address part of Register)
; (cdr x) x의 두번째 (Content of Decrement part of Register)
(define (GCD x y)
  (cond ((= y 0) (abs x))
        ((= x 0) (abs y))
        ((and (= x 0) (= y 0)) 1)
        (else (GCD y (remainder x y)))))

(define (make-rat x y)
  (let ((gcd (GCD x y)))
    (cond ((or (= x 0) (= y 0)) (cons 0 0))
          ((< y 0) (cons (* -1 (/ x gcd)) (* -1 (/ y gcd))))
          (else  (cons (/ x gcd) (/ y gcd))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

; (make-rat <n> <d>) 분자가 n, 분모가 d인 유리수 return
; (numer <x>) <x>의 분자 return
; (denom <x>) <x>의 분모 return
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (mult-rat x (make-rat (denom y) (numer x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

