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
(struct integer datatype (width) #:transparent)

(struct signed integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (l/signed v (integer-width t))))

; Parameterized data types are exposed as functions
; whose result is a type.
(define (signed-signature n)
  (type))

(define (unsigned-signature n)
  (type))

(struct unsigned integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (l/unsigned v (integer-width t))))

(struct tuple datatype (elt-types) #:transparent)

(struct union datatype (types) #:transparent)

(struct array datatype (sizes elt-type) #:transparent)

(struct record datatype (fields) #:transparent)

(struct range datatype (type) #:transparent)

; The boolean type is used internally.
(struct boolean datatype () #:transparent)

; The type of types.
(struct type datatype () #:transparent)

; Standard derived types.

(define-syntax bit (meta/builtin-function #'bit-impl))

(define (bit-impl)
  (unsigned 1))

(define (bit-impl-signature)
  (type))

; Type helpers.

(define (literal-type x)
  (unless (base/integer? x)
    (error "Cannot determine type of literal" x))
  (if (>= x 0)
    (unsigned (l/min-unsigned-width x))
    (signed   (l/min-signed-width   x))))
