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
(define (signed-signature tn)
  (match tn
    [(static-value n) (type (signed n))]
    [_                (error "Signed width must be a static value")]))

(define (unsigned-signature tn)
  (match tn
    [(static-value n) (type (unsigned n))]
    [_                (error "Unsigned width must be a static value")]))

(struct unsigned integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (l/unsigned v (integer-width t))))

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

; A type to defer the type inference of static values.
(struct static-value datatype (actual)
  #:transparent
  #:property prop:procedure (λ (t v) ((literal-type (static-value-actual t)) v)))

; Standard derived types.

(define-syntax bit (meta/builtin-function #'bit-impl))

(define (bit-impl)
  (unsigned 1))

(define (bit-impl-signature)
  (type (bit-impl)))

; Type helpers.

(define (literal-type x)
  (unless (base/integer? x)
    (error "Cannot determine type of literal" x))
  (if (>= x 0)
    (unsigned (l/min-unsigned-width x))
    (signed   (l/min-signed-width   x))))

(define (actual-type t)
  (match t
    [(static-value n) (literal-type n)]
    [_                t]))

(define (subtype? t u)
  (match (cons t u)
    [(cons (signed n)     (signed m))   (<= n m)]
    [(cons (unsigned n)   (unsigned m)) (<= n m)]
    [(cons (signed n)     (unsigned m)) #f]
    [(cons (unsigned n)   (signed m))   (<  n m)]
    [(cons (union vs)     _)            (for/and ([it (in-list vs)])
                                          (subtype? it u))]
    ; TODO tuple, array, record
    [_ #f]))
