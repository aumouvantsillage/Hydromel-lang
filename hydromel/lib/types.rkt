; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  (prefix-in base/ racket/base)
  (prefix-in l/ "logic.rkt")
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide (all-defined-out))

; A data type can be used as a conversion function.
; The default behavior is to return the given data unchanged.
(struct datatype ()
  #:transparent
  #:property prop:procedure (λ (t v) v))

(struct any datatype () #:transparent)

; Abstract type for signed and unsigned integers.
(struct abstract-integer datatype (width) #:transparent)

(struct signed abstract-integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (l/signed v (abstract-integer-width t))))

; Parameterized data types are exposed as functions
; whose result is a type.
(define (signed-signature tn)
  (match tn
    [(static-data n _) (type (signed n))]
    [_                 (error "Signed width must be a static integer" tn)]))

(define (unsigned-signature tn)
  (match tn
    [(static-data n _) (type (unsigned n))]
    [_                 (error "Unsigned width must be a static integer" tn)]))

(struct unsigned abstract-integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (l/unsigned v (abstract-integer-width t))))

(struct tuple datatype (elt-types) #:transparent)

(struct union datatype (types) #:transparent)

(struct array datatype (sizes elt-type) #:transparent)

(struct record datatype (fields) #:transparent)

; The range type is used internally.
(struct range datatype (type) #:transparent)

; The boolean type is used internally.
(struct boolean datatype () #:transparent)

; The type of types.
(struct type datatype (supertype) #:transparent)

; A type to keep a literal value with its type.
(struct static-data datatype (value type)
  #:transparent
  #:property prop:procedure (λ (t v) ((static-data-type t) v)))

; Standard derived types.

(define-syntax bit (meta/builtin-function #'bit-impl))

(define (bit-impl)
  (unsigned 1))

(define (bit-impl-signature)
  (type (bit-impl)))

(define-syntax natural (meta/builtin-function #'natural-impl))

(define (natural-impl)
  (unsigned #f))

(define (natural-impl-signature)
  (type (natural-impl)))

(define-syntax integer (meta/builtin-function #'integer-impl))

(define (integer-impl)
  (signed #f))

(define (integer-impl-signature)
  (type (integer-impl)))

; Type helpers.

(define (literal-type x)
  (unless (base/integer? x)
    (error "Cannot determine type of literal" x))
  (if (>= x 0)
    (unsigned (l/min-unsigned-width x))
    (signed   (l/min-signed-width   x))))

(define (actual-type t)
  (match t
    [(static-data n (unsigned #f)) (unsigned (l/min-unsigned-width n))]
    [(static-data n (signed   #f)) (signed   (l/min-unsigned-width n))]
    [(static-data _ t)             (actual-type t)]
    [(union ts)                    (foldl merge-types #f (map actual-type ts))]
    [(type t)                      (type (actual-type t))]
    [_                             t]))

(define (merge-types t u)
  (match (cons t u)
    [(cons _            #f)           t]
    [(cons (unsigned m) (unsigned n)) (unsigned (max m n))]
    [(cons (signed   m) (signed   n)) (signed   (max m n))]
    [(cons (unsigned m) (signed   n)) (signed   (max (add1 m) n))]
    [(cons (signed   m) (unsigned n)) (signed   (max m (add1 n)))]
    [_                                (error "types cannot be merged" t u)]))

(define (subtype? t u)
  (match (cons (actual-type t) (actual-type u))
    [(cons (signed n)   (signed m))   (<= n m)]
    [(cons (unsigned n) (unsigned m)) (<= n m)]
    [(cons (signed n)   (unsigned m)) #f]
    [(cons (unsigned n) (signed m))   (<  n m)]
    [(cons (union vs)   _)            (for/and ([it (in-list vs)])
                                        (subtype? it u))]
    [(cons _            (union vs))   (for/or ([it (in-list vs)])
                                        (subtype? t it))]
    ; TODO tuple, array, record
    [_ #f]))
