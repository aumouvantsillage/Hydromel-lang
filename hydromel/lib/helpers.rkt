#lang racket

(require
  syntax/parse/define
  "std.rkt"
  "slot.rkt"
  "signal.rkt"
  (prefix-in t/ "types.rkt"))

(provide
  slot-ref
  slot-set!
  slot-table
  print-slot-table)

(define-syntax-parser slot-ref*
  [(slot-ref* x)                    #'x]
  [(slot-ref* x f:identifier i ...) #'(slot-ref* (dict-ref   x 'f) i ...)]
  [(slot-ref* x n:number i ...)     #'(slot-ref* (vector-ref x n)  i ...)])

(define-syntax-parse-rule (slot-ref path ...)
  (slot-data (slot-ref* path ...)))

(define-syntax-parse-rule (slot-set! (path ...) value)
  (set-slot-data! (slot-ref* path ...) value))

(define (slot-table inst [parent #hash()] [path #f])
  (match inst
    [(slot sig _) #:when sig
     (hash-set parent path inst)]

    [(hash-table _ ...)
     (for/fold ([res parent])
               ([(k v) (in-dict inst)])
       (define name (symbol->string k))
       (define path^ (if path (string-append path "." name) name))
       (slot-table v res path^))]

    [(vector elt ...)
     (for/fold ([res parent])
               ([v (in-list elt)]
                [n (in-naturals)])
       (define index (format "[~a]" n))
       (define path^ (if path (string-append path index) index))
       (slot-table v res path^))]

    [(t/abstract-integer _)
     (hash-set parent path (slot (signal inst) (thunk (t/literal-type inst))))]

    [_
     (error "Unsupported data type at" path)]))

(define (print-slot-table tbl duration)
  (for ([(name slt) (in-dict tbl)])
    (define data (slot-data slt))
    (printf "~a : ~v = ~a\n"
      name
      (t/actual-type (slot-type slt))
      (if (signal? data)
        (signal-take data duration)
        data))))
