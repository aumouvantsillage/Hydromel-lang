; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
     ; Get the list of spliced port names.
     ; Include '* as a key to avoid when processing the hash-table.
     (define sp (cons '* (dict-ref inst '* (thunk empty))))
     ; For each field that is not a spliced port...
     (for/fold ([res parent])
               ([(k v) (in-dict inst)]
                #:when (not (member k sp)))
       ; Add the field name to the current path.
       (define name (symbol->string k))
       (define path^ (if path (string-append path "." name) name))
       ; Process the current entry and add it to the slot table.
       (slot-table v res path^))]

    [(vector elt ...)
     ; For each index and each element of the vector...
     (for/fold ([res parent])
               ([v (in-list elt)]
                [n (in-naturals)])
       ; Add the index to the current path.
       (define index (format "[~a]" n))
       (define path^ (if path (string-append path index) index))
       ; Process the current entry and add it to the slot table.
       (slot-table v res path^))]

    [_
     (error "Unsupported data type at" path inst)]))

(define (print-slot-table tbl duration)
  (for ([(name slt) (in-dict tbl)])
    (define data (slot-data slt))
    (printf "~a : ~v = ~a\n"
      name
      (t/normalize-type (slot-type slt))
      (if (signal? data)
        (signal-take data duration)
        data))))
