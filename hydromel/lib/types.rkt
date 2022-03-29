; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/hash
  (only-in data/collection length)
  data/pvector
  (prefix-in base/ racket/base)
  (prefix-in num/ "numeric.rkt")
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide (all-defined-out))

; A data type can be used as a conversion function.
; The default behavior is to return the given data unchanged.
(struct datatype ()
  #:transparent
  #:property prop:procedure (λ (t v) v))

(struct any datatype () #:transparent)

(struct none datatype () #:transparent)

; Abstract type for signed and unsigned integers.
(struct abstract-integer datatype (width) #:transparent)

(struct signed abstract-integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (num/signed v (abstract-integer-width t))))

; Parameterized data types are exposed as functions
; whose result is a type.
(define (signed:return-type tn)
  (match tn
    [(static-data n _) (static-data (signed n) (type:impl))]
    [_                 (error "Signed width must be a static integer" tn)]))

(define (unsigned:return-type tn)
  (match tn
    [(static-data n _) (static-data (unsigned n) (type:impl))]
    [_                 (error "Unsigned width must be a static integer" tn)]))

(struct unsigned abstract-integer ()
  #:transparent
  #:property prop:procedure (λ (t v) (num/unsigned v (abstract-integer-width t))))

(struct tuple datatype (elt-types) #:transparent)

(struct union datatype (types)
  #:transparent
  #:property prop:procedure (λ (t v) ((normalize-type t) v)))

(struct array datatype (size elt-type) #:transparent)

(define (array:return-type tn te)
  (match (list tn te)
    [(list (static-data n _) (static-data t _)) (static-data (array n t) (type:impl))]
    [_                                          (error "Cannot determine array type" tn te)]))

(struct record datatype (fields) #:transparent)

(define-syntax make-record (meta/make-function #'make-record:impl))

(define (make-record:impl . kv)
  (record (apply hash kv)))

; TODO make-record:impl:return-type

; The range type is used internally.
(struct range datatype (type) #:transparent)

; The boolean type is used internally.
(struct boolean datatype () #:transparent)

; The symbol type is used internally.
(struct symbol datatype (value) #:transparent)

; Subtype of a given type.
(struct subtype datatype (supertype) #:transparent)

; A type to keep a literal value with its type.
(struct static-data datatype (value type)
  #:transparent
  #:property prop:procedure (λ (t v) ((static-data-type t) v)))

(define (static-data/literal value)
  (static-data value (literal-type value)))

; Standard derived types.

(define-syntax type (meta/make-function #'type:impl))

(define type:impl (const (subtype (any))))

(define type:impl:return-type (const (static-data (type:impl) (type:impl))))

(define any:return-type (const (static-data (any) (type:impl))))

(define-syntax bit (meta/make-function #'bit:impl))

(define bit:impl (const (unsigned 1)))

(define bit:impl:return-type (const (static-data (bit:impl) (type:impl))))

(define-syntax natural (meta/make-function #'natural:impl))

(define natural:impl (const (unsigned #f)))

(define natural:impl:return-type (const (static-data (natural:impl) (type:impl))))

(define-syntax integer (meta/make-function #'integer:impl))

(define integer:impl (const (signed #f)))

(define integer:impl:return-type (const (static-data (integer:impl) (type:impl))))

(define-syntax enumeration (meta/make-function #'enumeration:impl))

(define (enumeration:impl . syms)
  (union (map symbol syms)))

; TODO enumeration:impl:return-type

; Type helpers.

(define (resize t w)
  (match (normalize-type t)
    [(signed _)   (signed w)]
    [(unsigned _) (unsigned w)]
    [_ (error "Resize expects integer type. Found " (format-type t))]))

(define (literal-type x)
  (cond [(base/symbol? x)
         (symbol x)]
        [(datatype? x)
         (type:impl)]
        [(base/integer? x)
         (if (>= x 0)
           (unsigned (num/min-unsigned-width x))
           (signed   (num/min-signed-width   x)))]
        [(pvector? x)
         (array (length x) (union (for/list ([v x]) (literal-type v))))]
        [(hash? x)
         (record (for/hash ([(k v) (in-dict x)])
                   (values k (literal-type v))))]
        [(list? x)
         (tuple (map literal-type x))]
        [else
         (error "Cannot determine the type of literal" x)]))

; Normalize a type t. Here we are only interested in concrete data types,
; which excludes (subtype _).
(define (normalize-type t)
  (match t
    ; If t represents a static integer value with unbounded width,
    ; return an integer type with minimal width.
    [(static-data n (unsigned #f)) (unsigned (num/min-unsigned-width n))]
    [(static-data n (signed   #f)) (signed   (num/min-signed-width   n))]
    ; If t represents any other static value, return its normalized type.
    [(static-data _ t)             (normalize-type t)]
    ; If t is a union type, return the common supertype of its normalized alternatives.
    ; TODO using foldl will not produce a minimal supertype. Maybe sort/partition ts according to <:
    [(union ts)                    (foldl common-supertype (none) (map normalize-type ts))]
    ; If t is an array type, normalize its element type.
    [(array n t)                   (array n (normalize-type t))]
    ; If t is a tuple type, normalize its element types.
    [(tuple ts)                    (tuple (map normalize-type ts))]
    ; If t is a record, normalize its field types.
    [(record fs)                   (record (for/hash ([(k v) (in-dict fs)])
                                             (values k (normalize-type v))))]
    ; In all other cases, the type is already normalized.
    [_                             t]))

; Return the upper bound of two types.
; It must be the "simplest" type r such that t <: r and u <: r.
(define (common-supertype t u)
  (match (list t u)
    ; If one of the arguments is none, return the other.
    [(list _              (none))         t]
    [(list (none)         _)              u]
    ; For integer types, return an integer type with
    ; appropriate signedness and the required width.
    [(list (unsigned m)   (unsigned n))   (unsigned (max m n))]
    [(list (signed   m)   (signed   n))   (signed   (max m n))]
    [(list (unsigned m)   (signed   n))   (signed   (max (add1 m) n))]
    [(list (signed   m)   (unsigned n))   (signed   (max m (add1 n)))]
    ; Arrays have a common supertype when they have the same length.
    ; The result is an array type wose element type is the common supertype
    ; of the element types of t and u.
    [(list (array    n v) (array    m w))
     #:when (= n m)                       (array n (common-supertype/normalize v w))]
    ; Symbol types have a common supertype when they refer to the same symbol.
    [(list (symbol   q)   (symbol   r))
     #:when (equal? q r)                  t]
    ; The common supertype of two record types contain fields with the
    ; intersection of their names, and the common supertypes of their types.
    [(list (record ft)    (record   fu))  (record (hash-intersect ft fu #:combine common-supertype/normalize))]
    ; This function expects t and u to be normalized.
    ; If they were based on unions before normalization, they should already
    ; have been reduced to non-union types.
    ; If we still get unions at this point, it means that t or u could not be
    ; reduced to simpler types, so we return a union here as well.
    [(list (union    ts)  (union    us))  (union (append ts us))]
    [(list (union    ts)  _)              (union (cons u ts))]
    [(list _              (union    us))  (union (cons t us))]
    ; If a common supertype could not be found, return a union of t and u.
    [_                                    (union (list t u))]))

(define (common-supertype/normalize t u)
  (common-supertype (normalize-type t) (normalize-type u)))

(define (<: t u)
  (define t^ (normalize-type t))
  (define u^ (normalize-type u))
  (match (list t^ u^)
    ; All types are subtypes of any.
    [(list _                    (any))         #t]
    ; none is a subtype of all types.
    [(list (none)               _)             #t]
    ; All integer types are subtypes of the unbounded signed type.
    [(list (abstract-integer n) (signed #f))   #t]
    ; All unsigned types are subtypes of the unbounded unsigned type.
    [(list (unsigned n)         (unsigned #f)) #t]
    ; A shorter integer type is a subtype of a longer integer type.
    ; If they have the same signedness, they can be of the same length.
    ; A signed type cannot be a subtype of an unsigned type.
    [(list (signed n)           (signed m))    (<= n m)]
    [(list (unsigned n)         (unsigned m))  (<= n m)]
    [(list (unsigned n)         (signed m))    (<  n m)]
    ; A symbol type can only be a subtype of itself.
    [(list (symbol ts)          (symbol us))   (equal? ts us)]
    ; An array type t is a subtype of an array type u if they have the same length
    ; and if the element type of t is a subtype of the element type of u.
    [(list (array n v)          (array m w))   (and (= n m) (<: v w))]
    ; A tuple type t is a subtype of a tuple type u if all the element types of t
    ; are subtypes of the element types of u.
    [(list (tuple ts)           (tuple us))    (and (<= (length ts) (length us))
                                                    (for/and ([it (in-list ts)]
                                                              [iu (in-list us)])
                                                      (<: it iu)))]
    ; A union type t is a subtype of a type u if all its alternatives are subtypes of u.
    [(list (union ts)           _)             (for/and ([it (in-list ts)])
                                                 (<: it u^))]
    ; A type t is a subtype of a union type u if t is a subtype of at least one alternative of u.
    [(list _                    (union us))    (for/or ([it (in-list us)])
                                                 (<: t^ it))]
    ; A record type t is a subtype of a record type u if all field names in t
    ; exist in u, and field types in t are subtypes of the corresponding field types in u.
    [(list (record ft)          (record fu))   (for/and ([(k v) (in-dict ft)])
                                                 (and (dict-has-key? fu k)
                                                      (<: v (dict-ref fu k))))]
    ; Subtyping between types.
    [(list (subtype ta)         (subtype tb))  (<: ta tb)]
    [_ #f]))

(define (format-type t)
  (define t^ (normalize-type t))
  (match t^
    [(signed   n) (format "signed(~a)" n)]
    [(unsigned n) (format "unsigned(~a)" n)]
    [(array    n v) (format "array(~a, ~a)" n (format-type v))]
    [(union    ts)  (format "union(~a)" (for/fold ([acc ""])
                                                  ([it (in-list ts)])
                                          (define s (format-type it))
                                          (if (positive? (string-length acc))
                                            (string-append acc ", " s)
                                            s)))]
    [(tuple    ts)  (format "tuple(~a)" (for/fold ([acc #f])
                                                  ([it (in-list ts)])
                                          (define s (format-type it))
                                          (if (string? acc)
                                            (string-append acc ", " s)
                                            s)))]
    [(record fs) (format "record(~a)" (for/fold ([acc #f])
                                                ([(k v) (in-dict fs)])
                                        (define s (format "~a: ~a" k (format-type v)))
                                        (if (string? acc)
                                          (string-append acc ", " s)
                                          s)))]
    [(symbol s) (format "~~~a" s)]
    [_ (format "~v" t^)]))
