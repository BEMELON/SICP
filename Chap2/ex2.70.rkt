#lang sicp
(#%require "ex2.69.rkt" "ex2.68.rkt")

(define sample-sets '((a 2) (Get 2) (Sha 3) (Wah 1) (boom 1) (job 2) (na 16) (yip 9)))
(define sample-string   '(Get a job
                        Sha na na na na na na na na
                        Get a job
                        Sha na na na na na na na na
                        Wah yip yip yip yip yip yip yip yip yip
                        Sha boom))
                    
(define huffman-tree (generate-huffman-tree sample-sets))
(define result (encode sample-string huffman-tree))
(display result) (newline)
(display (length result)) (newline)

; How many bits are required for the encoding? What is the
; smallest number of bits that would be needed to encode this
; song if we used a fixed-length code for the eight-symbol
; alphabet?

; 84 bits need. / if fixed-elngth code we need 108 bits