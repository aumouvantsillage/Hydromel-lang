; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/hash
  (only-in data/collection length)
  data/pvector
  "numeric.rkt")

(provide
  (struct-out any-type)
  (struct-out abstract-integer-type) (struct-out signed-type) (struct-out unsigned-type)
  (struct-out tuple-type) (struct-out array-type) (struct-out record-type) (struct-out union-type)
  (struct-out range-type) (struct-out symbol-type) (struct-out boolean-type)
  (struct-out subtype-type) (struct-out const-type)
  minimize resize type->string <:
  type-of make-const-type common-supertype)

; A data type can be used as a conversion function.
; The default behavior is to return the given data unchanged.
(struct abstract-type ()
  #:transparent
  #:property prop:procedure (λ (t v) v))

; The supertype of all types.
(struct any-type abstract-type () #:transparent)

; Abstract type for signed and unsigned integers.
(struct abstract-integer-type abstract-type (width) #:transparent)

(struct signed-type abstract-integer-type ()
  #:transparent
  #:property prop:procedure (λ (t v) (signed v (abstract-integer-type-width t))))

(struct unsigned-type abstract-integer-type ()
  #:transparent
  #:property prop:procedure (λ (t v) (unsigned v (abstract-integer-type-width t))))

; A tuple type contains a list of element types.
(struct tuple-type abstract-type (elt-types) #:transparent)

; A union type contains a list of alternative types.
(struct union-type abstract-type (alt-types)
  #:transparent
  #:property prop:procedure (λ (t v) ((minimize t) v)))

; An array type has a fixed size and an element type.
(struct array-type abstract-type (size elt-type) #:transparent)

; A record type contains a hash table that maps symbols to field types.
(struct record-type abstract-type (fields) #:transparent)

; The range type is used internally. It contains the type of the values in that range.
(struct range-type abstract-type (type) #:transparent)

; The boolean type is used internally.
(struct boolean-type abstract-type () #:transparent)

; The symbol type is used internally. It refers to a symbol.
(struct symbol-type abstract-type (value) #:transparent)

; The subtype of a given type.
(struct subtype-type abstract-type (supertype) #:transparent)

; A type to keep a literal value with its type.
(struct const-type abstract-type (value type)
  #:transparent
  #:property prop:procedure (λ (t v) ((const-type-type t) v)))

; Make a const-type whose type field is computed from the value.
(define (make-const-type value)
  (const-type value (type-of value)))

; Return the actual type of a const-type.
(define (normalize-const-type t)
  (match t
    [(const-type n (unsigned-type #f)) (unsigned-type (min-unsigned-width n))]
    [(const-type n (signed-type   #f)) (signed-type   (min-signed-width   n))]
    [(const-type _ u)                  u]
    [_                                 t]))

; Resize an integer type t to the given width w.
(define (resize t w)
  (match (minimize t)
    [(signed-type   _) (signed-type   w)]
    [(unsigned-type _) (unsigned-type w)]
    [_ (error "resize expects integer type. Found " (type->string t))]))

; Get the type of a given value x.
(define (type-of x)
  (cond [(symbol? x)        (symbol-type x)]
        [(abstract-type? x) (subtype-type (any-type))]
        [(integer? x)       (if (>= x 0)
                              (unsigned-type (min-unsigned-width x))
                              (signed-type   (min-signed-width   x)))]
        [(pvector? x)       (array-type (length x)
                                        (union-type (for/list ([v x])
                                                      (type-of v))))]
        [(hash? x)          (record-type (for/hash ([(k v) (in-dict x)])
                                           (values k (type-of v))))]
        [(list? x)          (tuple-type (map type-of x))]
        [else               (error "Cannot determine the type of literal" x)]))

; Minimize a type t.
(define (minimize t)
  (match t
    [(const-type _ _) (minimize (normalize-const-type t))]
    ; TODO using foldl will not always produce a minimal supertype. Maybe sort/partition ts according to <:
    [(union-type ts)  (foldl common-supertype (union-type empty) (map minimize ts))]
    [(array-type n t) (array-type n (minimize t))]
    [(tuple-type ts)  (tuple-type (map minimize ts))]
    [(record-type fs) (record-type (for/hash ([(k v) (in-dict fs)])
                                     (values k (minimize v))))]
    [(range-type t)   (range-type (minimize t))]
    [(subtype-type t) (subtype-type (minimize t))]
    [_                t]))

; Return the upper bound of two types.
; The result is not minimized.
(define (common-supertype t u)
  (match (list t u)
    [(list (const-type _ _)  _)                 (common-supertype (normalize-const-type t) u)]
    [(list _                 (const-type _ _))  (common-supertype t (normalize-const-type u))]
    ; For integer types, return an integer type with
    ; appropriate signedness and the required width.
    [(list (unsigned-type m) (unsigned-type n)) (unsigned-type (max m n))]
    [(list (signed-type   m) (signed-type   n)) (signed-type   (max m n))]
    [(list (unsigned-type m) (signed-type   n)) (signed-type   (max (add1 m) n))]
    [(list (signed-type   m) (unsigned-type n)) (signed-type   (max m (add1 n)))]
    ; Equal symbol types are their common supertype.
    ; We do not return the empty symbol type if q and r are different because it
    ; would break enumerations.
    [(list (symbol-type q) (symbol-type r)) #:when (equal? q r) t]
    ; The supertype of two collections is a collection that has the smallest size,
    ; or the common set of keys for records.
    [(list (array-type n v) (array-type m w))   (array-type (min n m) (common-supertype v w))]
    [(list (tuple-type ts)  (tuple-type us))    (tuple-type (map common-supertype ts us))]
    [(list (record-type ft) (record-type fu))   (record-type (hash-intersect ft fu #:combine common-supertype))]
    ; When one operand is the empty union, the common supertype is the other.
    [(list (union-type '()) _)                  u]
    [(list _                (union-type '()))   t]
    ; The common supertype of two unions is a union with their alternatives concatenated.
    ; We want to preserve the ordering here.
    ; We do not attempt to deduplicate the entries in ts and us here.
    ; This should be the role of minimize.
    [(list (union-type ts)  (union-type us))    (union-type (append us ts))]
    [(list (union-type ts)  _)                  (union-type (cons   u  ts))]
    [(list _                (union-type us))    (union-type (append us (list t)))]
    ; If a common supertype could not be found, return a union of t and u.
    [_                                          (union-type (list u t))]))

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
  (define t^ (minimize t))
  (match t^
    [(signed-type   n)   (format "signed(~a)" n)]
    [(unsigned-type n)   (format "unsigned(~a)" n)]
    [(array-type    n v) (format "array(~a, ~a)" n (type->string v))]
    [(union-type    ts)  (format "union(~a)" (for/fold ([acc ""])
                                                       ([it (in-list ts)])
                                               (define s (type->string it))
                                               (if (positive? (length acc))
                                                 (string-append acc ", " s)
                                                 s)))]
    [(tuple-type    ts)  (format "tuple(~a)" (for/fold ([acc ""])
                                                       ([it (in-list ts)])
                                               (define s (type->string it))
                                               (if (> (length acc) 0)
                                                 (string-append acc ", " s)
                                                 s)))]
    [(record-type fs)    (format "record(~a)" (for/fold ([acc ""])
                                                        ([(k v) (in-dict fs)])
                                                (define s (format "~a: ~a" k (type->string v)))
                                                (if (> (length acc) 0)
                                                  (string-append acc ", " s)
                                                  s)))]
    [(symbol-type s)     (format "~~~a" s)]
    [_                   (format "~v" t^)]))
