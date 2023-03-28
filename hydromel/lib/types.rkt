; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/hash
  (only-in data/collection length)
  data/pvector
  "numeric.rkt"
  "errors.rkt")

(provide
  (struct-out any-type)
  (struct-out abstract-integer-type) (struct-out signed-type) (struct-out unsigned-type)
  (struct-out tuple-type) (struct-out array-type) (struct-out record-type) (struct-out union-type)
  (struct-out range-type) (struct-out symbol-type) (struct-out boolean-type)
  (struct-out subtype-type) (struct-out const-type)
  minimize resize type->string <:
  type-of make-const-type common-supertype
  current-typecheck-stx assert-<: assert-const)

; This is the base struct for all built-in data types.
; A data type can be used as a conversion function.
; The default behavior is to return the given data unchanged.
(struct abstract-type ()
  #:transparent
  #:property prop:procedure (λ (t v) v))

; `any` is the supertype of all types.
(struct any-type abstract-type ()
  #:transparent)

; Integer types are used to represent integer numbers and bit vectors.
; `abstract-integer` is the common supertype of the `signed` and `unsigned` types.
; The `width` field contains the number of bits.
; If `width` is #f, the number of bits is unbounded.
(struct abstract-integer-type abstract-type (width)
  #:transparent)

; `signed` is the type of integer numbers represented in two's complement.
; Converting a value `v` to `signed(n)` consists in:
; * taking the `n` least significant bits of `v`,
; * interpreting them as a two's complement value,
; * returning the corresponding integer.
; See "numeric.rkt" for details.
(struct signed-type abstract-integer-type ()
  #:transparent
  #:property prop:procedure (λ (t v) (signed v (abstract-integer-type-width t))))

; `unsigned` is the type of natural numbers represented in pure binary.
; Converting a value `v` to `unsigned(n)` consists in:
; * taking the `n` least significant bits of `v`,
; * interpreting them as a natural number in pure binary,
; * returning the corresponding integer.
; See "numeric.rkt" for details.
(struct unsigned-type abstract-integer-type ()
  #:transparent
  #:property prop:procedure (λ (t v) (unsigned v (abstract-integer-type-width t))))

; A `tuple` type is a collection type with fixed size and heterogeneous
; element types.
(struct tuple-type abstract-type (elt-types)
  #:transparent)

; A `union` type allows to specify a list of alternative types that can be
; consumed or produced in a given context.
; FIXME: as a conversion function, a `union` type will recurse forever
; if it cannot be minimized to a concrete type.
(struct union-type abstract-type (alt-types)
  #:transparent
  #:property prop:procedure (λ (t v) ((minimize t) v)))

; An `array` type has a fixed size and a single element type.
; Multidimensional arrays are implemented as arrays of arrays.
(struct array-type abstract-type (size elt-type)
  #:transparent)

; A `record` type contains a hash table that maps symbols to field types.
(struct record-type abstract-type (fields)
  #:transparent)

; A `range` type is used internally to represent the type of expressions
; of the form `a .. b` such as in slices, comprehensions and loops.
; A `range` type is a wrapper for the type of the integers between `a` and `b`.
; This type is not directly available in the language.
(struct range-type abstract-type (type)
  #:transparent)

; The `boolean` type is used internally to represent the type of expressions
; used as conditions.
; This type is not directly available in the language.
(struct boolean-type abstract-type ()
  #:transparent)

; A `symbol` type is a singleton type for a symbol value.
; The "empty" symbol (with `value` = #f), is the supertype of all symbol types.
; This type is not directly available in the language.
; Users can manipulate symbol types via `enumeration` types,
; which are unions of symbol types.
(struct symbol-type abstract-type (value)
  #:transparent)

; `subtype` allows to declare a type that is a subtype of another type.
; For instance `T : subtype(U)` means that `T` is a type and `T <: U`.
; The type of all types is `subtype(any)`.
(struct subtype-type abstract-type (supertype)
  #:transparent)

; A `const` type represents the type of a compile-time value.
; It is typically used when the type of an expression depends on the values
; of some operands. In this situation, we need to access these operands' values
; at the type level.
; This type is not directly available in the language.
(struct const-type abstract-type (value type)
  #:transparent
  #:property prop:procedure (λ (t v) ((const-type-type t) v)))

; Make a const-type whose type field is computed from the value.
(define (make-const-type value)
  (const-type value (type-of value)))

; Return the actual type of a const-type.
(define (const-type-collapse t)
  (match t
    [(const-type n (unsigned-type #f))        (unsigned-type (min-unsigned-width n))]
    [(const-type n (signed-type   #f))        (signed-type   (min-signed-width   n))]
    [(const-type t (subtype-type (any-type))) (subtype-type t)]
    [(const-type _ u)                         u]
    [_                                        t]))

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

; Flatten the content of a union type.
(define (union-type-flatten ts)
  (flatten (for/list ([it (in-list ts)])
             (match it
               [(union-type us) (union-type-flatten us)]
               [_               it]))))

; Minimize the content of a union type.
; For each iteration, this function finds the types in (rest lst)
; that have a common supertype with (first lst).
; All those types are removed from lst.
; The common supertype is added to res.
(define (union-type-minimize* res ts)
  (match ts
    ; If there is zero or one remaining type to process, return.
    [(list)   res]
    [(list u) (cons u res)]
    ; If there are at least two remaining types,
    ; find a common supertype between u and elements of us.
    ; Proceed from the end to preserve the order in enumerations.
    [(list us ... u)
     (for/foldr ([v  u]     ; The current common supertype.
                 [vs empty] ; The list of remaining types from us.
                 #:result (union-type-minimize* (cons v res) vs))
                ([it (in-list us)])
      ; If v has no common supertype with the current type,
      ; keep v and add the current type to the list of remaining types.
      ; Otherwise, replace v with the common supertype.
      (define c (common-supertype it v))
      (if (union-type? c)
        (values v (cons it vs))
        (values c  vs)))]))

(define (union-type-minimize ts)
  (match (union-type-minimize* empty (union-type-flatten ts))
    [(list u) u]
    [us       (union-type us)]))

; Minimize a type t.
(define (minimize t)
  (match t
    [(const-type _ _) (minimize (const-type-collapse t))]
    [(union-type ts)  (union-type-minimize ts)]
    [(array-type n t) (array-type n (minimize t))]
    [(tuple-type ts)  (tuple-type (map minimize ts))]
    [(record-type fs) (record-type (for/hash ([(k v) (in-dict fs)])
                                     (values k (minimize v))))]
    [(range-type t)   (range-type (minimize t))]
    [(subtype-type t) (subtype-type (minimize t))]
    [_                t]))

; Return the upper bound of two types.
; The result is not minimized.
(define/match (common-supertype t u)
  [((const-type _ _)  _)                 (common-supertype (const-type-collapse t) u)]
  [(_                 (const-type _ _))  (common-supertype t (const-type-collapse u))]
  ; For integer types, return an integer type with
  ; appropriate signedness and the required width.
  [((unsigned-type m) (unsigned-type n)) (unsigned-type (max m n))]
  [((signed-type   m) (signed-type   n)) (signed-type   (max m n))]
  [((unsigned-type m) (signed-type   n)) (signed-type   (max (add1 m) n))]
  [((signed-type   m) (unsigned-type n)) (signed-type   (max m (add1 n)))]
  ; Equal symbol types are their common supertype.
  ; We do not return the empty symbol type if q and r are different
  ; because it would break enumerations.
  [((symbol-type s)   (symbol-type s))   t]
  ; The supertype of two collections is a collection that has the smallest size,
  ; or the common set of keys for records.
  [((array-type n v)  (array-type m w))  (array-type (min n m) (common-supertype v w))]
  [((tuple-type ts)   (tuple-type us))   (let ([l (min (length ts) (length us))])
                                           (tuple-type (map common-supertype (take ts l) (take us l))))]
  [((record-type ft)  (record-type fu))  (record-type (hash-intersect ft fu #:combine common-supertype))]
  ; When one operand is the empty union, the common supertype is the other.
  [((union-type '())  _)                 u]
  [(_                 (union-type '()))  t]
  ; The common supertype of two unions is a union with their alternatives concatenated.
  ; We want to preserve the ordering here.
  ; We do not attempt to deduplicate the entries in ts and us:
  ; this should be the role of minimize.
  [((union-type ts)   (union-type us))   (union-type (append us ts))]
  [((union-type ts)   _)                 (union-type (cons   u  ts))]
  [(_                 (union-type us))   (union-type (append us (list t)))]
  ; If a common supertype could not be found, return a union of t and u.
  [(_                 _)                 (union-type (list u t))])

; Check whether a type `t` is a subtype of a type `u`
(define/match (<: t u)
  ; All types are subtypes of any.
  [(_                         (any-type))         #t]
  ; Unwrap const types.
  [(_                         (const-type _ _))   (<: t (const-type-collapse u))]
  [((const-type _ _)          _)                  (<: (const-type-collapse t) u)]
  ; All integer types are subtypes of the unbounded signed type.
  ; All unsigned types are subtypes of the unbounded unsigned type.
  [((abstract-integer-type _) (signed-type   #f)) #t]
  [((unsigned-type         _) (unsigned-type #f)) #t]
  ; A shorter integer type is a subtype of a longer integer type.
  ; If they have the same signedness, they can be of the same length.
  ; A signed type cannot be a subtype of an unsigned type.
  [((signed-type   n)         (signed-type   m))  (<= n m)]
  [((unsigned-type n)         (unsigned-type m))  (<= n m)]
  [((unsigned-type n)         (signed-type   m))  (<  n m)]
  ; A symbol type can only be a subtype of the empty symbol or itself.
  [((symbol-type s)           (symbol-type #f))   #t]
  [((symbol-type s)           (symbol-type s))    #t]
  ; An array type t is a subtype of an array type u if t is at least as long as u
  ; and if the element type of t is a subtype of the element type of u.
  [((array-type n v)          (array-type m w))   (and (>= n m) (<: v w))]
  ; A tuple type t is a subtype of a tuple type u if t is at least as long as u
  ; and corresponding element types in t are subtypes of the element types in u.
  [((tuple-type ts)           (tuple-type us))    (and (>= (length ts) (length us))
                                                       (for/and ([it (in-list ts)]
                                                                 [iu (in-list us)])
                                                         (<: it iu)))]
  ; A union type t is a subtype of a type u if all its alternatives are subtypes of u.
  ; A type t is a subtype of a union type u if t is a subtype of at least one alternative of u.
  [((union-type ts)           _)                  (for/and ([it (in-list ts)])
                                                    (<: it u))]
  [(_                         (union-type us))    (for/or ([it (in-list us)])
                                                    (<: t it))]
  ; A record type t is a subtype of a record type u if all field names in u
  ; exist in t, and field types in t are subtypes of the corresponding field types in u.
  [((record-type tkv)         (record-type ukv))  (for/and ([(k v) (in-dict ukv)])
                                                    (and (dict-has-key? tkv k)
                                                         (<: (dict-ref tkv k) v)))]
  ; Subtypes are covariant.
  [((subtype-type ta)         (subtype-type tb))  (<: ta tb)]
  [(_                         _)                  #f])

; Convert a type `t` into a human-readable string representation.
(define (type->string t)
  (match (minimize t)
    [(any-type)                "any"]
    [(signed-type #f)          "integer"]
    [(unsigned-type #f)        "natural"]
    [(unsigned-type 1)         "bit"]
    [(signed-type n)           (format "signed(~a)" n)]
    [(unsigned-type n)         (format "unsigned(~a)" n)]
    [(array-type n v)          (format "array(~a, ~a)" n (type->string v))]
    [(union-type (list (symbol-type ss) ...))
     (format "enumeration(~a)" (string-join (for/list ([it (in-list ss)])
                                              (format "~~~a" it))
                                            ", "))]
    [(union-type ts)           (format "union(~a)" (string-join (map type->string ts) ", "))]
    [(tuple-type ts)           (format "tuple(~a)" (string-join (map type->string ts) ", "))]
    [(record-type fs)          (format "record(~a)" (string-join (for/list ([(k v) (in-dict fs)])
                                                                   (format "~a: ~a" k (type->string v)))
                                                                 ", "))]
    [(symbol-type s)           (format "symbol(~a)" s)]
    [(subtype-type (any-type)) "type"]
    [(subtype-type u)          (format "subtype(~a)" (type->string u))]
    [u                         (format "~v" u)]))

; A parameter with the current syntax object for error reporting during
; type inference or type-checking.
(define current-typecheck-stx (make-parameter #f))

; Check a list of subtyping relations.
; `ts` must have an even number of elements.
; If `ts` is of the form `(t0 u0 t1 u1 ...)`, this function evaluates `tn <: un`
; for each `n`. It the result is false, it raises a type error for position `pos + n`.
(define (assert-<: pos . ts)
  (match ts
    ['()
     (void)]
    [(list t u xs ...) #:when (<: t u)
     (apply assert-<: (and pos (add1 pos)) xs)]
    [(list t u _ ...)
     (raise-type-error (current-typecheck-stx) pos (type->string t) (type->string u))]
    [_
     (error "assert-<: expect an even number of arguments")]))

; Check whether a list of types are `const` types.
; This function inspects each type in `ts` and raises a semantic error when
; it finds a non-const type.
(define (assert-const pos . ts)
  (for ([(t n) (in-indexed ts)] #:unless (const-type? t))
    (raise-semantic-error "Expected constant value" (current-typecheck-stx) (+ pos n))))
