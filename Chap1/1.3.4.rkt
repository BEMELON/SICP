#lang sicp
(#%require "fixed-point.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average  (f x) x)))

(define (square x) (* x x))
; X |-> g(x)가 전구간 미분가능하다면, g(x) = 0 의 근ㅇㄴ 다음처럼 정의된 함수 x |-> f(x)의 정점과 같다.
; f(x) = x - ( g(x) / g'(x))
; Newton 방법은 f(x)의 고정점을 찾아서 방정식의 해를 어림잡는 방법.
(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 9)