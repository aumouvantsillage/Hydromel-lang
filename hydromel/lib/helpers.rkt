#lang racket

(require
  syntax/parse/define
  "std.rkt"
  "slot.rkt"
  "signal.rkt"
  (prefix-in t/ "types.rkt"))

(provide
  port-ref
  port-set!
  signal-table)

(define-syntax-parser port-ref*
  [(port-ref* x)                    #'x]
  [(port-ref* x f:identifier i ...) #'(port-ref* (dict-ref   x 'f) i ...)]
  [(port-ref* x n:number i ...)     #'(port-ref* (vector-ref x n)  i ...)])

(define-syntax-parse-rule (port-ref path ...)
  (slot-data (port-ref* path ...)))

(define-syntax-parse-rule (port-set! (path ...) value)
  (set-slot-data! (port-ref* path ...) value))

(define (signal-table inst [parent #hash()] [path #f])
  (match inst
    [(slot sig _) #:when sig
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

    [(? integer?)
     (hash-set parent path (slot (signal inst) (thunk (t/literal-type inst))))]

    [_
     (error "Unsupported data type at" path)]))
