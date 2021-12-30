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
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide
  int->bool      int->bool:impl         int->bool:impl:return-type
  _if_           if:impl                if:impl:return-type
  _case_         case:impl              case:impl:return-type
  _not_                                 bitwise-not:return-type
  _and_                                 bitwise-and:return-type
  _or_                                  bitwise-ior:return-type
  _xor_                                 bitwise-xor:return-type
  _==_           eq:impl                eq:impl:return-type
  _/=_           ne:impl                ne:impl:return-type
  _>_            gt:impl                gt:impl:return-type
  _<_            lt:impl                lt:impl:return-type
  _>=_           ge:impl                ge:impl:return-type
  _<=_           le:impl                le:impl:return-type
  _+_                                   +:return-type
  _-_                                   -:return-type
  _*_                                   *:return-type
  _/_                                   quotient:return-type
  _<<_                                  arithmetic-shift:return-type
  _>>_           arithmetic-shift-right arithmetic-shift-right:return-type
  _range_        range:impl             range:impl:return-type
  _slice_                               unsigned-slice:return-type
  _concat_       concat:impl            concat:impl:return-type
  _array_                               pvector:return-type
  _record_                              hash:return-type
  _nth_                                 nth:return-type
  _set_nth_                             set-nth:return-type
  _field_                               dict-ref:return-type
  signed_width                          min-signed-width:return-type
  unsigned_width                        min-unsigned-width:return-type
  _cast_         cast:impl              cast:impl:return-type
  (all-from-out  "numeric.rkt"))

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax int->bool (meta/make-function #'int->bool:impl))

(define (int->bool:impl a)
  (not (zero? a)))

(define (int->bool:impl:return-type ta)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to _if_ as if it were a function.
(define-syntax _if_ (meta/make-function #'if:impl))

(define-syntax-parse-rule (if:impl (~seq cnd thn) ... els)
  (cond [(int->bool:impl cnd) thn]
        ...
        [else els]))

(define-syntax-parse-rule (if:impl:return-type (~seq tc tt) ... te)
  (t/union (list tt ... te)))

; The Hydromel case statement is expanded to a call-expr
; to _case_ as if it were a function.
(define-syntax _case_ (meta/make-function #'case:impl))

(define-syntax-parser case:impl
  [(_ expr (~seq ch thn) ...)
   #'(let ([x expr])
       (cond [(member x ch) thn] ...))]
  [(_ expr (~seq ch thn) ... els)
   #'(let ([x expr])
       (cond [(member x ch) thn] ... [else els]))])

(define-syntax-parser case:impl:return-type
  [(_ tx (~seq tc tt) ...)
   #'(t/union (list tt ...))]
  [(_ tx (~seq tc tt) ... te)
   #'(t/union (list tt ... te))])

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/make-function #'min-unsigned-width))

(define (min-unsigned-width:return-type ta)
  (define ta^ (t/normalize-type ta))
  (t/unsigned (t/abstract-integer-width ta^)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/make-function #'min-signed-width))

(define (min-signed-width:return-type ta)
  (define ta^ (t/normalize-type ta))
  (t/unsigned (match ta^
                [(t/signed   n) n]
                [(t/unsigned n) (add1 n)]
                [_ (error "Cannot compute data size.")])))

; Boolean operators are all bitwise.
(define-syntax _not_ (meta/make-function/cast #'bitwise-not))
(define-syntax _and_ (meta/make-function      #'bitwise-and))
(define-syntax _or_  (meta/make-function      #'bitwise-ior))
(define-syntax _xor_ (meta/make-function      #'bitwise-xor))

(define (bitwise:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na)          (t/unsigned nb))          (t/unsigned (max na nb))]
    [(list (t/signed   na)          (t/abstract-integer  nb)) (t/signed   (max na nb))]
    [(list (t/abstract-integer  na) (t/signed   nb))          (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define bitwise-not:return-type identity)
(define bitwise-and:return-type bitwise:return-type)
(define bitwise-ior:return-type bitwise:return-type)
(define bitwise-xor:return-type bitwise:return-type)

; Comparison operations return integers 0 and 1.
(define-syntax _==_ (meta/make-function #'eq:impl))
(define-syntax _/=_ (meta/make-function #'ne:impl))
(define-syntax _>_  (meta/make-function #'gt:impl))
(define-syntax _<_  (meta/make-function #'lt:impl))
(define-syntax _>=_ (meta/make-function #'ge:impl))
(define-syntax _<=_ (meta/make-function #'le:impl))

(define (eq:impl a b)
  (if (equal? a b) 1 0))

(define (ne:impl a b)
  (if (equal? a b) 0 1))

(define (gt:impl a b)
  (if (> a b) 1 0))

(define (lt:impl a b)
  (if (< a b) 1 0))

(define (ge:impl a b)
  (if (>= a b) 1 0))

(define (le:impl a b)
  (if (<= a b) 1 0))

(define (comparison:return-type ta tb)
  (t/unsigned 1))

(define eq:impl:return-type comparison:return-type)
(define ne:impl:return-type comparison:return-type)
(define gt:impl:return-type comparison:return-type)
(define lt:impl:return-type comparison:return-type)
(define ge:impl:return-type comparison:return-type)
(define le:impl:return-type comparison:return-type)

; Use the built-in arithmetic operators.
(define-syntax _+_ (meta/make-function #'+))
(define-syntax _-_ (meta/make-function #'-))
(define-syntax _*_ (meta/make-function #'*))
(define-syntax _/_ (meta/make-function #'quotient))

(define (+:return-type ta tb)
  (define tr (t/common-supertype/normalize ta tb))
  (match tr
    [(t/abstract-integer w) (t/resize tr (add1 w))]
    [_ (error "Arithmetic operation expects integer operands." ta tb)]))

(define (-:return-type ta [tb #f])
  (if tb
    (+:return-type ta tb)
    (t/signed (add1 (t/abstract-integer-width (t/normalize-type ta))))))

(define (*:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na)         (t/unsigned nb))          (t/unsigned (+ na nb))]
    [(list (t/abstract-integer na) (t/signed   nb))          (t/signed   (+ na nb))]
    [(list (t/signed na)           (t/abstract-integer  nb)) (t/signed   (+ na nb))]
    [_ (error "Arithmetic operation expects integer operands.")]))

(define (quotient:return-type ta tb)
  ta)

(define-syntax _<<_ (meta/make-function #'arithmetic-shift))
(define-syntax _>>_ (meta/make-function #'arithmetic-shift-right))

(define (arithmetic-shift:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (match (list ta^ tb)
    [(list (t/abstract-integer na) (t/static-data nb _)) (t/resize ta^ (max 0 (+ na nb)))]
    [(list (t/abstract-integer na) (t/unsigned nb))      (t/resize ta^ (+ na (max-unsigned-value nb)))]
    [(list (t/abstract-integer na) (t/signed nb))        (t/resize ta^ (+ na (max-signed-value nb)))]
    [_ (error "Shift operation expects integer operands.")]))

(define (arithmetic-shift-right a b)
  (arithmetic-shift a (- b)))

(define (arithmetic-shift-right:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (match (list ta^ tb)
    [(list (t/abstract-integer na) (t/static-data nb _)) (t/resize ta^ (max 0 (- na nb)))]
    [(list (t/abstract-integer na) (t/unsigned nb))      ta^]
    [(list (t/abstract-integer na) (t/signed nb))        (t/resize ta^ (- na (min-signed-value nb)))]
    [_ (error "Shift operation expects integer operands.")]))

(define-syntax _range_ (meta/make-function #'range:impl))

; TODO Empty ranges are no longer supported.
; TODO Do we need an explicit "descending" range specifier?
(define (range:impl a b)
  (if (<= a b)
    (range a (add1 b))
    (range a (sub1 b) -1)))

(define (range:impl:return-type ta tb)
  (define tr (t/common-supertype/normalize ta tb))
  (unless (t/abstract-integer? tr)
    (error "Range expects integer boundaries."))
  (t/range tr))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
(define-syntax _slice_ (meta/make-function/cast #'unsigned-slice))

(define (unsigned-slice:return-type ta tb tc)
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
            (max 0 (add1 (- left right)))))

(define-syntax _concat_ (meta/make-function/cast #'concat:impl))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by concat:impl:return-type.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define-syntax-parse-rule (concat:impl (~seq a ta) ...)
  (unsigned-concat [a (~> ta t/normalize-type t/abstract-integer-width sub1) 0] ...))

(define (concat:impl:return-type . ts)
  (define ts^ (for/list ([it (in-list ts)]
                         [n (in-naturals)] #:when (odd? n))
                (~> it t/static-data-value t/normalize-type)))
  (define w (for/sum ([it (in-list ts^)])
              ; TODO assert that it contains an integer type
              (t/abstract-integer-width it)))
  (match (first ts^)
    [(t/signed _)   (t/signed w)]
    [(t/unsigned _) (t/unsigned w)]))

(define-syntax _array_ (meta/make-function #'pvector))

(define (pvector:return-type . ts)
  (t/array (length ts) (t/union ts)))

(define-syntax _record_ (meta/make-function #'hash))

(define (hash:return-type . ts)
  (define ts^ (for/list ([it (in-list ts)]
                         [n  (in-naturals)])
                (if (even? n)
                  (t/static-data-value it)
                  it)))
  (t/record (apply hash ts^)))

(define-syntax _nth_ (meta/make-function #'nth))

(define (nth:return-type ta tb)
  ; TODO check the type of tb
  (define ta^ (t/normalize-type ta))
  (match ta^
    [(t/array _ te) te]
    [_              (error "Not an array type" ta)]))

(define-syntax _set_nth_ (meta/make-function #'set-nth))

(define (set-nth:return-type ta tb tc)
  ; TODO check the types of tb and tc
  ta)

(define-syntax _field_ (meta/make-function #'dict-ref))

(define (dict-ref:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (unless (t/symbol? tb^)
    (error "Field identifier is not a symbol" tb^))
  (define field-name (t/symbol-value tb^))
  (unless (t/record? ta^)
    (error "Not a record type" ta^))
  (dict-ref (t/record-fields ta^) field-name
    (thunk (error "Unknown field" field-name))))

(define-syntax _cast_ (meta/make-function/cast #'cast:impl))

; cast does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(define (cast:impl a b)
  b)

(define (cast:impl:return-type ta tb)
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
     ta^]))
