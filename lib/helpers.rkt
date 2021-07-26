#lang racket

(require syntax/parse/define)

(provide
  port-ref
  port-set!
  signal-table)

(define-syntax-parser port-ref*
  [(port-ref* x)                    #'x]
  [(port-ref* x f:identifier i ...) #'(port-ref* (dict-ref   x 'f) i ...)]
  [(port-ref* x n:number i ...)     #'(port-ref* (vector-ref x n)  i ...)])

(define-syntax-parse-rule (port-ref path ...)
  (unbox (port-ref* path ...)))

(define-syntax-parse-rule (port-set! (path ...) value)
  (set-box! (port-ref* path ...) value))

(define (signal-table inst [parent #hash()] [path #f])
  (for/fold ([res parent])
            ([(k v) (in-dict inst)])
    (define name (symbol->string k))
    (define path^ (if path (string-append path "." name) name))
    (if (dict? v)
      (signal-table v res path^)
      (hash-set res path^ (unbox v)))))
