#lang sicp
(#%require "../util/util.rkt")

; ===========================================================
;              SCHEME_NUMBER - PACKAGE
; ===========================================================
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (tag (= x y))))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  '([LOG][DONE]install-scheme-number-package))

; ===========================================================
;              RATIONAL - PACKAGE
; ===========================================================
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
	          
  (define (equ? x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y))))
  
  (define (tag x) (attach-tag 'rational x))
  
  (define (=zero? x) (= (numer x) 0))
  
  (put '=zero? '(rational) =zero?)
  (put 'equ? '(rational rational) equ?)
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  '([LOG][DONE]install-rational-package))


; ===========================================================
;              POLAR - PACKAGE
; ===========================================================
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y)))
                                                (atan y x)))
                                              
  (define (equ? x y) (and (= (magnitude x) (magnitude y))
                          (= (angle x) (angle y))))
  
  (define (=zero? x) (= (magnitude x) 0))
                        
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put '=zero? '(polar) =zero?)
  (put 'equ? '(polar polar) equ?)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  '([LOG][DONE]install-polar-package))

; ===========================================================
;              RECTANGULAR - PACKAGE
; ===========================================================
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z))
                                 (square (imag-part z)))))
                               
  (define (angle z) (atan (imag-part z) (real-part z)))
  
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  
  (define (equ? x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y))))
  
  (define (=zero? x) (and (= (real-part x) 0)
                          (= (imag-part x) 0)))
                        
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  
  (put '=zero? '(rectangular) =zero?)
  (put 'equ? '(rectangular rectangular) equ?)
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  '([LOG][DONE]install-rectangular-package))

; ===========================================================
;              COMPLEX - PACKAGE
; ===========================================================
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
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (angle z) (apply-generic 'angle z))
  
  ; =====================================================
  ; Exercise 2.79
  (define (equ? x y) (apply-generic 'equ? x y))
  ; =====================================================
  
   ; =====================================================
  ; Exercise 2.80
  (define (=zero? x) (apply-generic '=zero? x))
  ; =====================================================
  
  ; interfaces
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put '=zero? '(complex) =zero?)
  (put 'equ? '(complex complex) equ?)
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  '([LOG][DONE]install-complex-package))


; =================================================================
;                     Generic-arithmetic - PACKAGES
; =================================================================
(define (install-generic-arithmetic-package)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  '([LOG][DONE]install-generic-arithmetic-package))


; =================================================================
;                         INSTALL - PACKAGES
; =================================================================
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-generic-arithmetic-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))

; =====================================================================================
; Exercise 2.77
; =====================================================================================

; [DEBUG] <apply-generic>
; (define (apply-generic op . args)
;   (display "[DBG][START] apply-generic\n")
;   (display "    <apply-generic> op : ") (display op) (newline)
;   (display "    <apply-generic> args : ") (display args) (newline)
;   (let ((type-tags (map type-tag args)))
;       (display "    <apply-generic> type-tags : ")
;       (display type-tags) (newline)
;     (let ((proc (get op type-tags)))
;       (display "    <apply-generic> proc : ")
;       (display proc) (newline)
;       (if (not (null? proc))
;         (apply proc (map contents args))
;         (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))


(define real_x (make-from-real-imag 0 0))
(define real_y (make-from-real-imag 10 5))
;(equ? real_x real_y)
;(=zero? real_x)

(define mag_x (make-from-mag-ang 0 0))
(define mag_y (make-from-mag-ang 10 6))
;(equ? mag_x mag_y)
;(=zero? mag_x)

(define num_x (make-scheme-number 10))
(define num_y (make-scheme-number 15))
;(equ? num_x num_y)
;(=zero? num_x)

(define x 10)
(define y 10)
;(equ? x y)
;(=zero? x)

(define rational_x (make-rational 0 15))
(define rational_y (make-rational 20 25))
; (equ? rational_x rational_y)
; (=zero? rational_x)

(define (number->complex x) (make-from-real-imag x 0))
(put-coercion 'scheme-number 'complex number->complex)
(add real_y num_x)
(add num_x num_y)