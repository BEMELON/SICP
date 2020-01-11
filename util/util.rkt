#lang sicp
(#%require (only racket/base current-inexact-milliseconds))

(#%provide runtime)
(define (runtime) (current-inexact-milliseconds))


