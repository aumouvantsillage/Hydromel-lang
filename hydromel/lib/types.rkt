; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/hash
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

; Standard derived types.

(define-syntax type (meta/make-function #'type:impl))

(define type:impl (const (subtype (any))))

(define type:impl:return-type (const (static-data (type:impl) (type:impl))))

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
  (match t
    [(signed _)   (signed w)]
    [(unsigned _) (unsigned w)]
    [_ (error "Resize expects integer type.")]))

(define (literal-type x)
  (cond [(base/symbol? x)
         (symbol x)]
        [(base/integer? x)
         (if (>= x 0)
           (unsigned (num/min-unsigned-width x))
           (signed   (num/min-signed-width   x)))]
        [else
         (error "Cannot determine type of literal" x)]))

(define (normalize-type t)
  (match t
    [(static-data n (unsigned #f)) (unsigned (num/min-unsigned-width n))]
    [(static-data n (signed   #f)) (signed   (num/min-unsigned-width n))]
    [(static-data _ t)             (normalize-type t)]
    [(union ts)                    (foldl common-supertype #f (map normalize-type ts))]
    [(subtype t)                   (subtype (normalize-type t))]
    [(array n t)                   (array n (normalize-type t))]
    [(tuple ts)                    (tuple (map normalize-type ts))]
    [(record fs)                   (record (for/hash ([(k v) (in-dict fs)])
                                             (values k (normalize-type v))))]
    [_                             t]))

(define (common-supertype t u)
  (match (list t u)
    [(list _              #f)             t]
    [(list (unsigned m)   (unsigned n))   (unsigned (max m n))]
    [(list (signed   m)   (signed   n))   (signed   (max m n))]
    [(list (unsigned m)   (signed   n))   (signed   (max (add1 m) n))]
    [(list (signed   m)   (unsigned n))   (signed   (max m (add1 n)))]
    [(list (array    n v) (array    m w))
     #:when (= n m)                       (array n (common-supertype v w))]
    [(list (symbol   q)   (symbol   r))
     #:when (equal? q r)                  t]
    [(list (record ft)    (record fu))    (record (hash-intersect ft fu
                                                                  #:combine common-supertype))]
    [(list (union    ts)  (union    us))  (union (append ts us))]
    [(list (union    ts)  _)              (union (cons u ts))]
    [(list _              (union    us))  (union (cons t us))]
    [_                                    (union (list t u))]))

(define (<: t u)
  (define t^ (normalize-type t))
  (define u^ (normalize-type u))
  (match (list t^ u^)
    [(list (abstract-integer n) (signed #f))   #t]
    [(list (unsigned n)         (unsigned #f)) #t]
    [(list (signed n)           (signed m))    (<= n m)]
    [(list (unsigned n)         (unsigned m))  (<= n m)]
    [(list (signed n)           (unsigned m))  #f]
    [(list (unsigned n)         (signed m))    (<  n m)]
    [(list (symbol ts)          (symbol us))   (equal? ts us)]
    [(list (array n v)          (array m w))   (and (= n m) (<: v w))]
    [(list (union ts)           _)             (for/and ([it (in-list ts)])
                                                 (<: it u^))]
    [(list _                    (union us))    (for/or ([it (in-list us)])
                                                 (<: t^ it))]
    [(list (record ft)          (record fu))   (for/and ([(k v) (in-dict ft)])
                                                 (and (dict-has-key? fu k)
                                                      (<: v (dict-ref fu k))))]
    ; TODO tuple, array
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
    [(symbol s) (symbol->string s)]
    [_ (format "~v" t^)]))
