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

; Get the content of the given instance at the given path.
; `path` is a list of symbols and integers that represent port names and indices.
(define (instance-ref* inst path)
  (match path
    ['()                      inst]
    [(list hd tl ...)         (instance-ref* (instance-ref* inst hd) tl)]
    [(? nonnegative-integer?) (vector-ref inst path)]
    [(? symbol?)              (dict-ref inst path)]
    [_                        (error "Invalid path" path)]))

; Get the slot value in the given instance at the given path.
(define (instance-ref inst path)
  (slot-data (instance-ref* inst path)))

; Assign a new value to a slot in the given instance at the given path.
(define (instance-set! inst path value)
  (set-slot-data! (instance-ref* inst path) value))

; Flatten a hierarchy of instances into a dictionary that maps slot paths to slots.
; The keys of the resulting dictionary are strings that concatenate port
; names and indices (e.g. "a.b[0].c").
; The table contains slots for ports, constants and local signals.
; Spliced ports are omitted.
; Only the non-empty slots are kept in the output.
(define (instance-slot-table inst [parent #hash()] [path #f])
  (match inst
    [(slot _ sig _ _) #:when sig
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

; Run the given instance for the given duration and print slot values.
; Each line of the output is of the form: "path : type = value" where
; `value` is either a single value (if the current slot contains a constant),
; or a list (if the current slot contains a signal).
; This function prints port values, constants and local signals across the
; hierarchy of instances that starts with `inst`.
; Spliced ports are omitted.
; Only the non-empty slots are kept in the output.
(define (instance-dump inst duration)
  (for ([(name slt) (in-dict (instance-slot-table inst))])
    (define data (slot-data slt))
    (printf "~a : ~a = ~a\n"
      name
      (type->string (slot-type slt))
      (if (signal? data)
        (signal-take data duration)
        data))))
