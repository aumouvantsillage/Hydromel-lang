; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/hash
  (only-in data/collection length nth)
  data/pvector
  (prefix-in base/ racket/base)
  (prefix-in num/ "numeric.rkt")
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide
  (struct-out any-type)
  (struct-out abstract-integer-type) (struct-out signed-type) (struct-out unsigned-type)
  (struct-out tuple-type) (struct-out array-type) (struct-out record-type) (struct-out union-type)
  (struct-out range-type) (struct-out symbol-type) (struct-out boolean-type)
  (struct-out subtype-type) (struct-out const-type)
  normalize resize type->string <:
  type-of-literal make-const-type common-supertype common-supertype/normalize)

; A data type can be used as a conversion function.
; The default behavior is to return the given data unchanged.
(struct abstract-type ()
  #:transparent
  #:property prop:procedure (λ (t v) v))

(struct any-type abstract-type () #:transparent)

; Abstract type for signed and unsigned integers.
(struct abstract-integer-type abstract-type (width) #:transparent)

(struct signed-type abstract-integer-type ()
  #:transparent
  #:property prop:procedure (λ (t v) (num/signed v (abstract-integer-type-width t))))

(struct unsigned-type abstract-integer-type ()
  #:transparent
  #:property prop:procedure (λ (t v) (num/unsigned v (abstract-integer-type-width t))))

(struct tuple-type abstract-type (elt-types) #:transparent)

(struct union-type abstract-type (types)
  #:transparent
  #:property prop:procedure (λ (t v) ((normalize t) v)))

(struct array-type abstract-type (size elt-type) #:transparent)

(struct record-type abstract-type (fields) #:transparent)

; The range type is used internally.
(struct range-type abstract-type (type) #:transparent)

; The boolean type is used internally.
(struct boolean-type abstract-type () #:transparent)

; The symbol type is used internally.
(struct symbol-type abstract-type (value) #:transparent)

; Subtype of a given type.
(struct subtype-type abstract-type (supertype) #:transparent)

; A type to keep a literal value with its type.
(struct const-type abstract-type (value type)
  #:transparent
  #:property prop:procedure (λ (t v) ((const-type-type t) v)))

(define (make-const-type value)
  (const-type value (type-of-literal value)))

; Type helpers.

(define (resize t w)
  (match (normalize t)
    [(signed-type _)   (signed-type w)]
    [(unsigned-type _) (unsigned-type w)]
    [_ (error "Resize expects integer type. Found " (type->string t))]))

(define (type-of-literal x)
  (cond [(symbol? x)        (symbol-type x)]
        [(abstract-type? x) (subtype-type (any-type))]
        [(integer? x)       (if (>= x 0)
                              (unsigned-type (num/min-unsigned-width x))
                              (signed-type   (num/min-signed-width   x)))]
        [(pvector? x)       (array-type (length x)
                                        (union-type (for/list ([v x])
                                                      (type-of-literal v))))]
        [(hash? x)          (record-type (for/hash ([(k v) (in-dict x)])
                                           (values k (type-of-literal v))))]
        [(list? x)          (tuple-type (map type-of-literal x))]
        [else               (error "Cannot determine the type of literal" x)]))

; Normalize a type t. Here we are only interested in concrete data types,
; which excludes (subtype-type _).
(define (normalize t)
  (match t
    ; If t represents a static integer value with unbounded width,
    ; return an integer type with minimal width.
    [(const-type n (unsigned-type #f)) (unsigned-type (num/min-unsigned-width n))]
    [(const-type n (signed-type   #f)) (signed-type   (num/min-signed-width   n))]
    ; If t represents any other static value, return its normalized type.
    [(const-type _ t)                  (normalize t)]
    ; If t is a union type, return the common supertype of its normalized alternatives.
    ; TODO using foldl will not produce a minimal supertype. Maybe sort/partition ts according to <:
    [(union-type ts)                   (foldl common-supertype (union-type empty)
                                                               (map normalize ts))]
    ; If t is an array type, normalize its element type.
    [(array-type n t)                  (array-type n (normalize t))]
    ; If t is a tuple type, normalize its element types.
    [(tuple-type ts)                   (tuple-type (map normalize ts))]
    ; If t is a record, normalize its field types.
    [(record-type fs)                  (record-type (for/hash ([(k v) (in-dict fs)])
                                                      (values k (normalize v))))]
    ; If t is a range, normalize its value type.
    [(range-type t)                    (range-type (normalize t))]
    ; In all other cases, the type is already normalized.
    [_                                 t]))

; Return the upper bound of two types.
; It must be the "simplest" type r such that t <: r and u <: r.
(define (common-supertype t u)
  (match (list t u)
    ; For integer types, return an integer type with
    ; appropriate signedness and the required width.
    [(list (unsigned-type m) (unsigned-type n)) (unsigned-type (max m n))]
    [(list (signed-type   m) (signed-type   n)) (signed-type   (max m n))]
    [(list (unsigned-type m) (signed-type   n)) (signed-type   (max (add1 m) n))]
    [(list (signed-type   m) (unsigned-type n)) (signed-type   (max m (add1 n)))]
    ; The result is an array type whose element type is the common supertype
    ; of the element types of t and u.
    [(list (array-type n v) (array-type m w))
     (array-type (min n m) (common-supertype/normalize v w))]
    ; TODO tuples
    ; Equal symbol types are their common supertype.
    ; We do not return the empty symbol if q and r are different because it
    ; would break enumerations.
    [(list (symbol-type q) (symbol-type r)) #:when (equal? q r)
     t]
    ; The common supertype of two record types contain fields with the
    ; intersection of their names, and the common supertypes of their types.
    [(list (record-type ft) (record-type fu))
     (record-type (hash-intersect ft fu #:combine common-supertype/normalize))]
    ; This function expects t and u to be normalized.
    ; If they were based on unions before normalization, they should already
    ; have been reduced to non-union types.
    ; If we still get unions at this point, it means that t or u could not be
    ; reduced to simpler types, so we return a union here as well.
    [(list (union-type '()) _)                u]
    [(list _                (union-type '())) t]
    [(list (union-type ts)  (union-type us))  (union-type (append us ts))]
    [(list (union-type ts)  _)                (union-type (cons   u  ts))]
    [(list _                (union-type us))  (union-type (append us (list t)))]
    ; If a common supertype could not be found, return a union of t and u.
    [_                                        (union-type (list u t))]))

(define (common-supertype/normalize t u)
  (common-supertype (normalize t) (normalize u)))

(define (<: t u)
  (match (list t u)
    ; All types are subtypes of any.
    [(list _  (any-type)) #t]
    ; Unwrap const types.
    [(list _                 (const-type _ u^)) (<: t u^)]
    [(list (const-type _ t^) _)                 (<: t^ u)]
    ; All integer types are subtypes of the unbounded signed type.
    ; All unsigned types are subtypes of the unbounded unsigned type.
    [(list (abstract-integer-type _) (signed-type   #f)) #t]
    [(list (unsigned-type         _) (unsigned-type #f)) #t]
    ; A shorter integer type is a subtype of a longer integer type.
    ; If they have the same signedness, they can be of the same length.
    ; A signed type cannot be a subtype of an unsigned type.
    [(list (signed-type   n) (signed-type   m)) (<= n m)]
    [(list (unsigned-type n) (unsigned-type m)) (<= n m)]
    [(list (unsigned-type n) (signed-type   m)) (<  n m)]
    ; A symbol type can only be a subtype of the empty symbol or itself.
    [(list (symbol-type ts) (symbol-type #f)) #t]
    [(list (symbol-type ts) (symbol-type us)) (equal? ts us)]
    ; An array type t is a subtype of an array type u if t is at least as long as u
    ; and if the element type of t is a subtype of the element type of u.
    [(list (array-type n v) (array-type m w)) (and (>= n m) (<: v w))]
    ; A tuple type t is a subtype of a tuple type u if t is at least as long as u
    ; and corresponding element types in t are subtypes of the element types in u.
    [(list (tuple-type ts) (tuple-type us)) (and (>= (length ts) (length us))
                                                 (for/and ([it (in-list ts)]
                                                           [iu (in-list us)])
                                                   (<: it iu)))]
    ; A union type t is a subtype of a type u if all its alternatives are subtypes of u.
    ; A type t is a subtype of a union type u if t is a subtype of at least one alternative of u.
    [(list (union-type ts) _) (for/and ([it (in-list ts)])
                                (<: it u))]
    [(list _ (union-type us)) (for/or ([it (in-list us)])
                                (<: t it))]
    ; A record type t is a subtype of a record type u if all field names in u
    ; exist in t, and field types in t are subtypes of the corresponding field types in u.
    [(list (record-type ft) (record-type fu)) (for/and ([(k v) (in-dict fu)])
                                                (and (dict-has-key? ft k)
                                                     (<: (dict-ref ft k) v)))]
    ; Subtyping between types.
    [(list (subtype-type ta) (subtype-type tb))  (<: ta tb)]
    [_ #f]))

(define (type->string t)
  (define t^ (normalize t))
  (match t^
    [(signed-type   n)   (format "signed(~a)" n)]
    [(unsigned-type n)   (format "unsigned(~a)" n)]
    [(array-type    n v) (format "array(~a, ~a)" n (type->string v))]
    [(union-type    ts)  (format "union(~a)" (for/fold ([acc ""])
                                                       ([it (in-list ts)])
                                               (define s (type->string it))
                                               (if (positive? (string-length acc))
                                                 (string-append acc ", " s)
                                                 s)))]
    [(tuple-type    ts)  (format "tuple(~a)" (for/fold ([acc ""])
                                                       ([it (in-list ts)])
                                               (define s (type->string it))
                                               (if (> (string-length acc) 0)
                                                 (string-append acc ", " s)
                                                 s)))]
    [(record-type fs)    (format "record(~a)" (for/fold ([acc ""])
                                                        ([(k v) (in-dict fs)])
                                                (define s (format "~a: ~a" k (type->string v)))
                                                (if (> (string-length acc) 0)
                                                  (string-append acc ", " s)
                                                  s)))]
    [(symbol-type s)     (format "~~~a" s)]
    [_                   (format "~v" t^)]))
