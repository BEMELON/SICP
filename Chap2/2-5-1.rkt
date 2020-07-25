#lang sicp
(#%require "../util/util.rkt")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (attach-tag tag object) (cons tag object))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  '([LOG][DONE]install-scheme-number-package))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  '([LOG][DONE]install-rational-package))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-polar-package)
  ;; internal procedures
  (define (square z) (* z z))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y)))
                                                (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  '([LOG][DONE]install-polar-package))

(define (install-complex-package)
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2))
						    (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2))
						    (- (imag-part z1) (imag-part z2))))
                                
  (define (mul-complex z1 z2) (make-from-mag-ang (* (magnitude z1) (magnitude z2))
						    (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2) (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
						    (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))

  ; =====================================================
  ; Exercise 2.77
  (define (magnitude z) (apply-generic 'magnitude z))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ; =====================================================
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  '([LOG][DONE]install-complex-package))

(install-complex-package)
(install-scheme-number-package)
(install-rational-package)
(install-polar-package)


; =====================================================================================
; Exercise 2.77
(put 'make-from-real-imag 'rectangular (lambda (x y) (cons 'rectangular (cons x y))))
(put 'make-from-mag-ang 'polar (lambda (x y) (cons 'polar (cons x y))))

; (define (apply-generic op . args)
;   (display "[DBG][START] apply-generic\n")
;   (display "    <apply-generic> op : ") (display op) (newline)
;   (display "    <apply-generic> args : ") (display args) (newline)
;   (let ((type-tags (map type-tag args)))
;       (display "    type-tags : ")
;       (display type-tags) (newline)
;     (let ((proc (get op type-tags)))
;       (display "    proc : ")
;       (display proc) (newline)
;       (if (not (null? proc))
;         (apply proc (map contents args))
;         (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))


(define z ((get 'make-from-mag-ang 'complex) 4 5))
(apply-generic 'magnitude z)

; =====================================================================================
