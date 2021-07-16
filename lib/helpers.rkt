#lang racket

(require syntax/parse/define)

(provide
  port-ref
  port-set!)

(define-syntax-parser port-ref*
  [(port-ref* x)                    #'x]
  [(port-ref* x f:identifier i ...) #'(port-ref* (f x) i ...)]
  [(port-ref* x n:number i ...)     #'(port-ref* (vector-ref x n) i ...)])

(define-syntax-parse-rule (port-ref path ...)
  (unbox (port-ref* path ...)))

(define-syntax-parse-rule (port-set! (path ...) value)
  (set-box! (port-ref* path ...) value))
