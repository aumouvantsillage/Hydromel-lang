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
(define (signed-return-type tn)
  (match tn
    [(static-data n _) (static-data (signed n) (type-impl))]
    [_                 (error "Signed width must be a static integer" tn)]))

(define (unsigned-return-type tn)
  (match tn
    [(static-data n _) (static-data (unsigned n) (type-impl))]
    [_                 (error "Unsigned width must be a static integer" tn)]))

(struct unsigned abstract-integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (l/unsigned v (abstract-integer-width t))))

(struct tuple datatype (elt-types) #:transparent)

(struct union datatype (types) #:transparent)

(struct array datatype (size elt-type) #:transparent)

(define (array-return-type tn te)
  (match (list tn te)
    [(list (static-data n _) (static-data t _)) (static-data (array n t) (type-impl))]
    [_                                          (error "Cannot determine array type" tn te)]))

(struct record datatype (fields) #:transparent)

; The range type is used internally.
(struct range datatype (type) #:transparent)

; The boolean type is used internally.
(struct boolean datatype () #:transparent)

; Subtype of a given type.
(struct subtype datatype (supertype) #:transparent)

; A type to keep a literal value with its type.
(struct static-data datatype (value type)
  #:transparent
  #:property prop:procedure (λ (t v) ((static-data-type t) v)))

; Standard derived types.

(define-syntax bit (meta/builtin-function #'bit-impl))

(define (bit-impl)
  (unsigned 1))

(define (bit-impl-return-type)
  (static-data (bit-impl) (type-impl)))

(define-syntax natural (meta/builtin-function #'natural-impl))

(define (natural-impl)
  (unsigned #f))

(define (natural-impl-return-type)
  (static-data (natural-impl) (type-impl)))

(define-syntax integer (meta/builtin-function #'integer-impl))

(define (integer-impl)
  (signed #f))

(define (integer-impl-return-type)
  (static-data (integer-impl) (type-impl)))

(define-syntax type (meta/builtin-function #'type-impl))

(define (type-impl)
  (subtype (any)))

(define (type-impl-return-type)
  (static-data (type-impl) (type-impl)))

; Type helpers.

(define (literal-type x)
  (unless (base/integer? x)
    (error "Cannot determine type of literal" x))
  (if (>= x 0)
    (unsigned (l/min-unsigned-width x))
    (signed   (l/min-signed-width   x))))

(define (normalize-type t)
  (match t
    [(static-data n (unsigned #f)) (unsigned (l/min-unsigned-width n))]
    [(static-data n (signed   #f)) (signed   (l/min-unsigned-width n))]
    [(static-data _ t)             (normalize-type t)]
    [(union ts)                    (foldl common-supertype #f (map normalize-type ts))]
    [(subtype t)                   (subtype (normalize-type t))]
    [(array n t)                   (array n (normalize-type t))]
    [_                             t]))

(define (common-supertype t u)
  (match (list t u)
    [(list _              #f)           t]
    [(list (unsigned m)   (unsigned n)) (unsigned (max m n))]
    [(list (signed   m)   (signed   n)) (signed   (max m n))]
    [(list (unsigned m)   (signed   n)) (signed   (max (add1 m) n))]
    [(list (signed   m)   (unsigned n)) (signed   (max m (add1 n)))]
    [(list (array    n v) (array    m w))
     #:when (= n m)                     (array n (common-supertype v w))]
    [_                                  (error "types cannot be merged" t u)]))

(define (<: t u)
  (match (list (normalize-type t) (normalize-type u))
    [(list (signed n)   (signed m))   (<= n m)]
    [(list (unsigned n) (unsigned m)) (<= n m)]
    [(list (signed n)   (unsigned m)) #f]
    [(list (unsigned n) (signed m))   (<  n m)]
    [(list (array n v)  (array m w))  (and (= n m) (<: v w))]
    [(list (union vs)   _)            (for/and ([it (in-list vs)])
                                        (<: it u))]
    [(list _            (union vs))   (for/or ([it (in-list vs)])
                                        (<: t it))]
    ; TODO tuple, array, record
    [_ #f]))
