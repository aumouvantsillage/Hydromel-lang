; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  "std.rkt"
  "slot.rkt"
  "signal.rkt"
  "types.rkt")

(provide
  instance-ref
  instance-ref*
  instance-set!
  instance-dump)

(define (instance-ref* inst path)
  (match path
    ['()                      inst]
    [(list hd tl ...)         (instance-ref* (instance-ref* inst hd) tl)]
    [(? nonnegative-integer?) (vector-ref inst path)]
    [(? symbol?)              (dict-ref inst path)]
    [_                        (error "Invalid path" path)]))

(define (instance-ref inst path)
  (slot-data (instance-ref* inst path)))

(define (instance-set! inst path value)
  (set-slot-data! (instance-ref* inst path) value))

(define (instance-slot-table inst [parent #hash()] [path #f])
  (match inst
    [(slot sig _ _) #:when sig
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
       (instance-slot-table v res path^))]

    [(vector elt ...)
     ; For each index and each element of the vector...
     (for/fold ([res parent])
               ([(v n) (in-indexed elt)])
       ; Add the index to the current path.
       (define index (format "[~a]" n))
       (define path^ (if path (string-append path index) index))
       ; Process the current entry and add it to the slot table.
       (instance-slot-table v res path^))]

    [_
     (error "Unsupported data type at" path inst)]))

(define (instance-dump inst duration)
  (for ([(name slt) (in-dict (instance-slot-table inst))])
    (define data (slot-data slt))
    (printf "~a : ~a = ~a\n"
      name
      (type->string (slot-type slt))
      (if (signal? data)
        (signal-take data duration)
        data))))
