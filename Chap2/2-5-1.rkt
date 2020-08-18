#lang sicp
(#%require "../util/util.rkt")

; =================================================================
;                     Generic-arithmetic - PACKAGES
; =================================================================
(define (install-generic-arithmetic-package)

  
  (define (not-null? object) (not (null? object)))                    
  (define (get-tower-level-by-type type)
    (cond ((eq? type 'scheme-number) 1)
          ((eq? type 'rational) 2)
          ((eq? type 'real) 3)
          ((eq? type 'complex) 4)
          (else (error "<get-tower-level-by-type> no matching type in type-tower : " (list type)))))

(define (get-tower-type-by-level level)
    (cond ((eq? level 1) 'scheme-number)
        ((eq? level 2) 'rational)
        ((eq? level 3) 'real)
        ((eq? level 4) 'complex)
        (else (error "<get-tower-type-by-level> no matching level in type-tower : " (list level)))))                
      
  ;=================================================================
  ; apply-generic : Multi operands compose of different types
  ;===========================================================================        
  (define (apply-generic op . args)
      ; =======================================================================
      ; internal fucntions
      ; =======================================================================
      (define (not-null? x) (not (null? x)))

            
      ; match? target , type-tag
      (define (find-type target type-tags args)
          (if (null? type-tags)
              target
              (let ((type1 (car type-tags))
                    (arg1 (car args)))
                (let ((target-coercion (get-coercion type1 target)))
                    (if (not-null? target-coercion)
                        (find-type target (cdr type-tags) (cdr args))
                        nil)))))
      
      ; Target looper
      (define (find-target type-tags args)
          (display "             <find-target> type-tags : ") (display type-tags) (newline)
          (display "             <find-target> args : ") (display args) (newline)
          (if (null? type-tags) 
              nil
              (let ((target (find-type (car type-tags) (cdr type-tags) (cdr args))))
                  (if (null? target) 
                      (find-target (cdr type-tags) (cdr args))
                      target))))
                   
      ; =======================================================================
      ; Exercise 2.84
      ; =======================================================================
      (define (find-highest-type type-tags)
          (define (search type-tags result)
              (if (null? type-tags)
                  result
                  (let ((level (get-tower-level-by-type (car type-tags))))
                      (if (< result level)
                          (search (cdr type-tags) level)
                          (search (cdr type-tags) result)))))
          (get-tower-type-by-level (search type-tags 0)))
                                                 
  
      (define (convert target args)
          (define (raise-until-target target arg)
              (if (eq? target (type-tag arg))
                  arg
                  (raise-until-target target (apply-generic 'raise arg))))
              
          (define (raise-args args)
              (if (null? args)
                  nil
                  (cons (raise-until-target target (car args))
                        (raise-args (cdr args)))))
          (raise-args args))
  
      ; =======================================================================
      ; procedures
      ; =======================================================================
      (display "[DBG][START] apply-generic\n")
      (display "    <apply-generic> op : ") (display op) (newline)
      (display "    <apply-generic> args : ") (display args) (newline)
      (let ((type-tags (map type-tag args)))
          (let ((proc (get op type-tags)))
              (display "    <apply-generic> type-tags : ") (display type-tags) (newline)
              (display "    <apply-generic> proc : ") (display proc) (newline)
              (if (not-null? proc)    
                  (apply proc (map contents args))
                  (let ((target (find-highest-type type-tags)))
                      (display "    <apply-generic> target : ") (display target) (newline)
                      (cond ((and (not-null? target) (not (= 1 (length args)))) (drop (apply apply-generic (append (list op) (convert target args)))))
                            ((= 1 (length args)) args)
                            (else (error "No method for These Types: APPLY-GENERIC" (list op type-tags)))))))))
                      
  ; ===========================================================================
  ; drop : drop datum until possible
  ; ===========================================================================        
  (define (drop datum)
    (display "    [DBG][START] drop\n")
    (display "\t\t<drop> datum : ") (display datum) (newline)
    (if (= 1 (get-tower-level-by-type (type-tag datum)))
        datum
        (begin
          (let ((dropped_datum (apply apply-generic (append (list 'project) (list datum)))))
               (let ((restored_datum (apply apply-generic (append (list 'raise) (list dropped_datum)))))
                     (let ((equ_proc (get 'equ? (list (type-tag datum) (type-tag restored_datum)))))
                          (if (not-null? equ_proc)
                              (if (apply apply-generic (append (list 'equ?) (list datum restored_datum)))
                                  (drop dropped_datum)
                                  datum)
                              datum)))))))
                      

  (put 'drop 'generic-arithmetic drop)
  (put 'apply-generic 'generic-arithmetic apply-generic)
  '([LOG][DONE]install-generic-arithmetic-package))

; ===========================================================
;              SCHEME-NUMBER - PACKAGE
; ===========================================================
(define (install-scheme-number-package)
  (define (tag x) 
    (attach-tag 'scheme-number x))
   
  ; Exercise 2.83
  (define (scheme-number->rational x)
    ((get 'make 'rational) x 1))
  
  (define (make-scheme-number x)
    (tag x))
    
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (tag (= x y))))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number make-scheme-number)
  (put 'raise '(scheme-number) scheme-number->rational)
  (put '=zero? 'scheme-number (lambda (x) (= x 0)))
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
  
  ; Exercise 2.83
  (define (rational->real datum) 
    (define (toFloatingPoint x) (* 1.0 x))
    ((get 'make 'real) (/ (toFloatingPoint (numer datum)) (denom datum))))
    
  ; Exercise 2.85
  (define (rational->scheme-number x) ((get 'make 'scheme-number) (round (/ (numer x) (denom x)))))
  
  (put 'project '(rational) rational->scheme-number)
  (put 'raise '(rational) rational->real)
  
  (put '=zero? 'rational =zero?)
  (put 'equ? '(rational rational) equ?)
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  '([LOG][DONE]install-rational-package))

; =====================================================
; REAL - PACKAGE
; =====================================================
(define (install-real-package)
  (define (real-part z) z)
  (define (equ? x y) (= x y))
  (define (=zero? x) (= x 0))
  (define (tag x) (attach-tag 'real x))
  (define (make-real x) (tag (* x 1.0)))
  (define (add-real x y) (+ x y))
  (define (sub-real x y) (- x y))
  (define (mul-real x y) (* x y))
  (define (div-real x y) (/ x y))
  
  ; Exercise 2.83
  (define (real->complex z)
    ((get 'make-from-real-imag 'complex) (real-part z) 0))
  
           
  (define (real->rational z)
    (let ((rat (rationalize (inexact->exact z) 1/100)))
         ((get 'make 'rational)
               (numerator rat)
               (denominator rat))))
    
  (put 'raise '(real) real->complex)
  (put 'project '(real) real->rational)
  
  (put 'make-real 'real make-real)
  (put 'real-part 'real real-part)
  (put '=zero? 'real =zero?)
  (put 'make 'real make-real)
  (put 'equ? '(real real) equ?)
  (put 'add '(real real) (lambda (z1 z2) (tag (add-real z1 z2))))
  (put 'sub '(real real) (lambda (z1 z2) (tag (sub-real z1 z2))))
  (put 'mul '(real real) (lambda (z1 z2) (tag (mul-real z1 z2))))
  (put 'div '(real real) (lambda (z1 z2) (tag (div-real z1 z2))))
  '([LOG][DONE]install-real-package))


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
  (put '=zero? 'polar =zero?)
  (put 'equ? '(polar polar) equ?)
  (put 'real-part 'polar real-part)
  (put 'imag-part 'polar imag-part)
  (put 'magnitude 'polar magnitude)
  (put 'angle 'polar angle)
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
  
  (put '=zero? 'rectangular =zero?)
  (put 'equ? '(rectangular rectangular) equ?)
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude 'rectangular magnitude)
  (put 'angle 'rectangular angle)
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

  ; Exercise 2.77
  (define (magnitude z) ((get 'apply-generic 'generic-arithmetic) 'magnitude z))
  (define (real-part z) ((get 'apply-generic 'generic-arithmetic) 'real-part z))
  (define (imag-part z) ((get 'apply-generic 'generic-arithmetic) 'imag-part z))
  (define (angle z) ((get 'apply-generic 'generic-arithmetic) 'angle z))
  
  ; Exercise 2.79
  (define (equ? x y) (apply-generic 'equ? x y))
  
  
  ; Exercise 2.80
  (define (=zero? x) (apply-generic '=zero? x))

  
  ; Exercise 2.84
  (define (complex->real z) ((get 'make 'real) (real-part z)))
  
  ; interfaces
  (put 'project '(complex) complex->real)
  
  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)
  (put '=zero? 'complex =zero?)
  (put 'equ? '(complex complex) equ?)
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  
  '([LOG][DONE]install-complex-package))

; =================================================================
;                         INSTALL - PACKAGES
; =================================================================
(install-generic-arithmetic-package)
(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)


(define (apply-repeat proc op args)
  (cond ((= (length args) 1) (car args))
        ((null? (cadr args)) (apply proc (append (list op) (list (car args) (cadr args)))))
        (else (apply-repeat proc op (append (list (apply proc (append (list op) (list (car args) (cadr args)))))
                                    (cddr args))))))

(define (add . args) (apply-repeat apply-generic 'add args))
(define (sub . args) (apply-repeat apply-generic 'sub args))
(define (mul . args) (apply-repeat apply-generic 'mul args))
(define (div . args) (apply-repeat apply-generic 'div args))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise datum) (apply-generic 'raise datum))
(define (project datum) ((get 'apply-generic 'generic-arithmetic) 'project datum))
(define (drop datum) ((get 'drop 'generic-arithmetic) datum))
(define (apply-generic op . args) (apply (get 'apply-generic 'generic-arithmetic) (append (list op) (list (car args) (cadr args)))))

(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang x y) ((get 'make-from-mag-ang 'complex) x y))
(define (make-real x) ((get 'make-real 'real) x))

(define (number->complex x) (make-from-real-imag x 0))
(define (number->rational x) (make-rational x 1))
(define (number->real x) (make-real x))
(put-coercion 'scheme-number 'complex number->complex)
(put-coercion 'scheme-number 'rational number->rational)
(put-coercion 'scheme-number 'real number->real)
(put-coercion 'rational 'real (get 'raise 'rational))

(define complex_x (make-from-real-imag 10 15))
(define complex_y (make-from-real-imag 10 0))

(define real_x (make-real 0))
(define real_y (make-real 10))

(define mag_x (make-from-mag-ang 0 0))
(define mag_y (make-from-mag-ang 10 6))

(define num_x (make-scheme-number 10))
(define num_y (make-scheme-number 15))

(define x 10)
(define y 10)

(define rational_x (make-rational 10 15))
(define rational_y (make-rational 20 1))


(add complex_y num_x)




