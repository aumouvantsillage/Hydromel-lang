; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "signal.rkt"
  "slot.rkt"
  (prefix-in t/ "types.rkt")
  (only-in "numeric.rkt"
    min-unsigned-width min-signed-width
    min-signed-value   max-signed-value
    min-unsigned-value max-unsigned-value
    unsigned-slice     unsigned-concat)
  syntax/parse/define
  threading
  (only-in data/collection
    nth set-nth)
  data/pvector
  (prefix-in meta/ "meta.rkt")
  (for-syntax
    (prefix-in meta/ "meta.rkt")
    racket/syntax
    syntax/parse/lib/function-header))

(provide
  _not_
  _slice_
  _concat_       concat:impl
  _cast_         cast:impl
  (all-from-out  "numeric.rkt"))

; ------------------------------------------------------------------------------
; Helpers for custom function definitions
; ------------------------------------------------------------------------------

(begin-for-syntax
  (define (function-impl-name name)
    (format-id name "~a:impl" name))

  (define (function-return-type-name name)
    (format-id name "~a:return-type" name)))

(define-syntax-parse-rule (declare-function name fn-name)
  (begin
    (provide name)
    (define-syntax name (meta/make-function #'fn-name))))

(define-syntax-parse-rule (declare-function/cast name fn-name)
  (begin
    (provide name)
    (define-syntax name (meta/make-function/cast #'fn-name))))

(define-syntax-parse-rule (define-function* name)
  #:with fn-name (function-impl-name #'name)
  #:with rt-name (function-return-type-name #'fn-name)
  (begin
    (declare-function name fn-name)
    (provide fn-name rt-name)))

(define-syntax-parser define-function
  #:literals [λ]
  [(_ name fn-name:id)
   #'(define-function name fn-name (const (t/none)))]

  [(_ name fn-name:id rt-fn)
   #'(begin
       (declare-function name fn-name)
       (define-return-type fn-name rt-fn))]

  [(_ name fn)
   #'(define-function name fn (const (t/none)))]

  [(_ name fn rt-fn)
   #:with fn-name (function-impl-name #'name)
   #'(begin
       (provide fn-name)
       (define fn-name fn)
       (define-function name fn-name rt-fn))])

(define-syntax-parse-rule (define-return-type name fn)
  #:with rt-name (function-return-type-name #'name)
  (begin
      (provide rt-name)
      (define rt-name (return-type-function name fn))))

(define-syntax-parser return-type-function
  #:literals [λ const]
  [(_ _ (~and fn (const body ...)))
   #'fn]

  [(_ name (λ (arg:id ...) body ...))
   #'(λ (arg ...)
       (define raw (let () body ...))
       (if (and (t/static-data? arg) ...)
         (t/static-data (name (t/static-data-value arg) ...) raw)
         raw))]

  [(_ name (λ args:id body ...))
   #'(λ args
       (define raw (let () body ...))
       (if (andmap t/static-data? args)
         (t/static-data (apply name (map t/static-data-value args)) raw)
         raw))]

  [(_ name (fn-name:id arg ...))
   #'(return-type-function name (λ (arg ...) (fn-name arg ...)))])

; ------------------------------------------------------------------------------
; Conditionals.
; ------------------------------------------------------------------------------

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-function int->bool
  (λ (a) (not (zero? a)))
  (const (t/boolean)))

; The Hydromel if statement is expanded to a call-expr to _if_.
(define-function* _if_)

(define-syntax-parse-rule (_if_:impl (~seq cnd thn) ... els)
  (cond [(int->bool:impl cnd) thn]
        ...
        [else els]))

(define (_if_:impl:return-type . ts)
  (match ts
    [(list tc tt te ...)
     (match tc
       ; If the first condition is static and true, return the type of the first "then" clause.
       [(t/static-data v _)
        (if (int->bool:impl v)
          tt
          (apply _if_:impl:return-type te))]
       ; If the first condition is not static, return a union of the type of the first "then" clause
       ; and the type of the rest.
       [_
        (t/union (list tt (apply _if_:impl:return-type te)))])]
    [(list te) te]))

; The Hydromel case statement is expanded to a call-expr to _case_.
(define-function* _case_)

(define-syntax-parse-rule (_case_:impl expr (~seq ch thn) ... (~optional els))
  #:with els^ (or (attribute els) #'(error "Value did not not match any choice"))
  (let ([v expr])
    (cond [(member v ch) thn]
          ...
          [else els^])))

(define (_case_:impl:return-type tx . ts)
  (match tx
    ; If the expression is static and true, inspect the cases for static choices.
    [(t/static-data v _)
     (apply _case_:impl:return-type/static v ts)]
    ; If the expression is not static, return a union of all target clauses.
    [_
     (define last-n (sub1 (length ts)))
     (t/union (for/list ([it (in-list ts)]
                         [n (in-naturals)]
                         #:when (or (odd? n) (= n last-n)))
                it))]))

(define (_case_:impl:return-type/static v . ts)
  (match ts
    [(list tc tt te ...)
     ; If the expression value matches a static choice, return the corresponding type.
     (define tc^ (filter t/static-data? tc))
     (if (member v (map t/static-data-value tc^))
       tt
       (apply _case_:impl:return-type/static v te))]
    [(list te) te]))

; ------------------------------------------------------------------------------
; Boolean and bitwise operations.
; ------------------------------------------------------------------------------

; Boolean operators are all bitwise.
(define-syntax _not_ (meta/make-function/cast #'bitwise-not))

(define bitwise-not:return-type identity)

(define (bitwise-return-type ta tb)
  (match (list (t/normalize-type ta) (t/normalize-type tb))
    [(list (t/unsigned na)          (t/unsigned nb))          (t/unsigned (max na nb))]
    [(list (t/signed   na)          (t/abstract-integer  nb)) (t/signed   (max na nb))]
    [(list (t/abstract-integer  na) (t/signed   nb))          (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define-function _and_ bitwise-and
  (bitwise-return-type ta tb))

(define-function _or_  bitwise-ior
  (bitwise-return-type ta tb))

(define-function _xor_ bitwise-xor
  (bitwise-return-type ta tb))

; ------------------------------------------------------------------------------
; Arithmetic operations.
; ------------------------------------------------------------------------------

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-function unsigned_width min-unsigned-width
  (λ (ta)
    (~> ta
        t/normalize-type
        t/abstract-integer-width
        t/unsigned)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-function signed_width min-signed-width
  (λ (ta)
    (match (t/normalize-type ta)
      [(t/signed   n) (t/unsigned n)]
      [(t/unsigned n) (t/unsigned (add1 n))]
      [_ (error "Cannot compute data size.")])))

; Comparison operations return integers 0 and 1.
(define-function _==_
  (λ (a b) (if (equal? a b) 1 0))
  (const (t/unsigned 1)))

(define-function _/=_
  (λ (a b) (if (equal? a b) 0 1))
  (const (t/unsigned 1)))

(define-function _>_
  (λ (a b) (if (> a b) 1 0))
  (const (t/unsigned 1)))

(define-function _<_
  (λ (a b) (if (< a b) 1 0))
  (const (t/unsigned 1)))

(define-function _>=_
  (λ (a b) (if (>= a b) 1 0))
  (const (t/unsigned 1)))

(define-function _<=_
  (λ (a b) (if (<= a b) 1 0))
  (const (t/unsigned 1)))

(define (add-sub-return-type ta tb)
  (define tr (t/common-supertype/normalize ta tb))
  (match tr
    [(t/abstract-integer w) (t/resize tr (add1 w))]
    [_ (error "Arithmetic operation expects integer operands." ta tb)]))

; Use the built-in arithmetic operators.
(define-function _+_ +
  (add-sub-return-type ta tb))

(define-function _-_ -
  (add-sub-return-type ta tb))

(define-function _*_ *
  (λ (ta tb)
    (match (list (t/normalize-type ta) (t/normalize-type tb))
      [(list (t/unsigned na)         (t/unsigned nb))          (t/unsigned (+ na nb))]
      [(list (t/abstract-integer na) (t/signed   nb))          (t/signed   (+ na nb))]
      [(list (t/signed na)           (t/abstract-integer  nb)) (t/signed   (+ na nb))]
      [_ (error "Arithmetic operation expects integer operands.")])))

(define-function _/_ quotient
  (λ (ta tb) ta))

(define-function _neg_
  (λ (a) (- a))
  (λ (ta)
    (~> ta
        t/normalize-type
        t/abstract-integer-width
        add1
        t/signed)))

(define-function _<<_ arithmetic-shift
  (λ (ta tb)
    (define ta^ (t/normalize-type ta))
    (match (list ta^ tb)
      [(list (t/abstract-integer na) (t/static-data nb _)) (t/resize ta^ (max 0 (+ na nb)))]
      [(list (t/abstract-integer na) (t/unsigned nb))      (t/resize ta^ (+ na (max-unsigned-value nb)))]
      [(list (t/abstract-integer na) (t/signed nb))        (t/resize ta^ (+ na (max-signed-value nb)))]
      [_ (error "Shift operation expects integer operands.")])))

(define-function _>>_
  (λ (a b)
    (arithmetic-shift a (- b)))
  (λ (ta tb)
    (define ta^ (t/normalize-type ta))
    (match (list ta^ tb)
      [(list (t/abstract-integer na) (t/static-data nb _)) (t/resize ta^ (max 0 (- na nb)))]
      [(list (t/abstract-integer na) (t/unsigned nb))      ta^]
      [(list (t/abstract-integer na) (t/signed nb))        (t/resize ta^ (- na (min-signed-value nb)))]
      [_ (error "Shift operation expects integer operands.")])))

; TODO Empty ranges are no longer supported.
; TODO Do we need an explicit "descending" range specifier?
(define-function _range_
  (λ (a b)
    (if (<= a b)
      (range a (add1 b))
      (range a (sub1 b) -1)))
  (λ (ta tb)
    (define tr (t/common-supertype/normalize ta tb))
    (unless (t/abstract-integer? tr)
      (error "Range expects integer boundaries."))
    (t/range tr)))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
(define-syntax _slice_ (meta/make-function/cast #'unsigned-slice))

(define-return-type unsigned-slice
  (λ (ta tb tc)
    (define left (match tb
                   [(t/static-data n _) n]
                   [(t/unsigned    n)   (max-unsigned-value n)]
                   [(t/signed      n)   (max-signed-value   n)]
                   [_                   (error "Invalid type for left slice index.")]))
    (define right (match tc
                   [(t/static-data n _) n]
                   [(t/unsigned    n)   (min-unsigned-value n)]
                   [(t/signed      n)   (min-signed-value   n)]
                   [_                   (error "Invalid type for right slice index.")]))
    (t/resize (t/normalize-type ta)
              (max 0 (add1 (- left right))))))

(define-syntax _concat_ (meta/make-function/cast #'concat:impl))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by concat:impl:return-type.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define (concat:impl . vs)
  (apply unsigned-concat
    (for/fold ([res empty]
               [lst vs]
               #:result res)
              ([n (in-naturals)])
              #:break (empty? lst)
      (define-values (l r) (split-at-right lst 2))
      (values
        (cons (list (first r) (~> r second t/normalize-type t/abstract-integer-width sub1) 0) res)
        l))))

(define-return-type concat:impl
  (λ ts
    (define ts^ (for/list ([it (in-list ts)]
                           [n (in-naturals)] #:when (odd? n))
                  (~> it t/static-data-value t/normalize-type)))
    (define w (for/sum ([it (in-list ts^)])
                ; TODO assert that it contains an integer type
                (t/abstract-integer-width it)))
    (match (first ts^)
      [(t/signed _)   (t/signed w)]
      [(t/unsigned _) (t/unsigned w)])))

; ------------------------------------------------------------------------------
; Array and record operations.
; ------------------------------------------------------------------------------

(define-function _array_ pvector
  (λ ts (t/array (length ts) (t/union ts))))

(define-function _record_ hash
  (λ ts
    (define ts^ (for/list ([it (in-list ts)]
                           [n  (in-naturals)])
                  (if (even? n)
                    (t/static-data-value it)
                    it)))
    (t/record (apply hash ts^))))

(define-function _nth_ nth
  (λ (ta tb)
    ; TODO check the type of tb
    (match (t/normalize-type ta)
      [(t/array _ te) te]
      [_              (error "Not an array type" ta)])))

(define-function _set_nth_ set-nth
  ; TODO check the types of tb and tc
  (λ (ta tb tc) ta))

(define-function _field_ dict-ref
  (λ (ta tb)
    (define ta^ (t/normalize-type ta))
    (define tb^ (t/normalize-type tb))
    (unless (t/symbol? tb^)
      (error "Field identifier is not a symbol" tb^))
    (define field-name (t/symbol-value tb^))
    (unless (t/record? ta^)
      (error "Not a record type" ta^))
    (dict-ref (t/record-fields ta^) field-name
      (thunk (error "Unknown field" field-name)))))

; ------------------------------------------------------------------------------
; Type operations.
; ------------------------------------------------------------------------------

(define-syntax _cast_ (meta/make-function/cast #'cast:impl))

; cast does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(define (cast:impl a b)
  b)

(define-return-type cast:impl
  (λ (ta tb)
    (define ta^ (t/normalize-type (t/static-data-value ta)))
    (define tb^ (t/normalize-type tb))
    (match ta^
      [(t/signed #f)
       (match tb^
         [(t/signed   _) tb^]
         [(t/unsigned n) (t/signed n)]
         [_ (error "Cannot cast value to signed")])]

      [(t/unsigned #f)
       (match tb^
         [(t/signed   n) (t/unsigned n)]
         [(t/unsigned _) tb^]
         [_ (error "Cannot cast value to unsigned")])]

      [_
       ta^])))
