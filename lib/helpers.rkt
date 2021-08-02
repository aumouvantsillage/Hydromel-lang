#lang racket

(require
  syntax/parse/define
  "std.rkt")

(provide
  port-ref
  port-set!
  signal-table)

(define-syntax-parser port-ref*
  [(port-ref* x)                    #'x]
  [(port-ref* x f:identifier i ...) #'(port-ref* (dict-ref   x 'f) i ...)]
  [(port-ref* x n:number i ...)     #'(port-ref* (vector-ref x n)  i ...)])

(define-syntax-parse-rule (port-ref path ...)
  (slot-signal (port-ref* path ...)))

(define-syntax-parse-rule (port-set! (path ...) value)
  (set-slot-signal! (port-ref* path ...) value))

(define (signal-table inst [parent #hash()] [path #f])
  (match inst
    [(slot sig) #:when sig
     (hash-set parent path inst)]

    [(hash-table _ ...)
     (for/fold ([res parent])
               ([(k v) (in-dict inst)])
       (define name (symbol->string k))
       (define path^ (if path (string-append path "." name) name))
       (signal-table v res path^))]

    [(vector elt ...)
     (for/fold ([res parent])
               ([v (in-list elt)]
                [n (in-naturals)])
       (define index (format "[~a]" n))
       (define path^ (if path (string-append path index) index))
       (signal-table v res path^))]

    [_
     (error "No signal at" path)]))
