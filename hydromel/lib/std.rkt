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
    (prefix-in meta/ "meta.rkt")
    racket/syntax
    syntax/parse/lib/function-header))

(provide
  int->bool      int->bool:impl         int->bool:impl:return-type
  _if_           if:impl                if:impl:return-type
  _case_         case:impl              case:impl:return-type
  _not_                                 bitwise-not:return-type
  _and_
  _or_
  _xor_
  _==_           eq:impl
  _/=_           ne:impl
  _>_            gt:impl
  _<_            lt:impl
  _>=_           ge:impl
  _<=_           le:impl
  _+_
  _-_
  _neg_          neg:impl
  _*_
  _/_
  _<<_
  _>>_           arithmetic-shift-right
  _range_        range:impl
  _slice_
  _concat_       concat:impl
  _array_
  _record_
  _nth_
  _set_nth_
  _field_
  signed_width
  unsigned_width
  _cast_         cast:impl
  (all-from-out  "numeric.rkt"))

(define-syntax-parser define-return-type
  [(_ (name arg:id ...) body ...)
   #:with rt-name (format-id #'name "~a:return-type" #'name)
   #'(begin
       (provide rt-name)
       (define (rt-name arg ...)
         (define raw (let () body ...))
         (if (and (t/static-data? arg) ...)
           (t/static-data (name (t/static-data-value arg) ...) raw)
           raw)))]
  [(_ (name . args) body ...)
   #:with rt-name (format-id #'name "~a:return-type" #'name)
   #'(begin
       (provide rt-name)
       (define (rt-name . args)
         (define raw (let () body ...))
         (if (andmap t/static-data? args)
           (t/static-data (apply name (map t/static-data-value args)) raw)
           raw)))])

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax int->bool (meta/make-function #'int->bool:impl))

(define (int->bool:impl a)
  (not (zero? a)))

(define (int->bool:impl:return-type ta)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr to _if_.
(define-syntax _if_ (meta/make-function #'if:impl))

(define-syntax-parse-rule (if:impl (~seq cnd thn) ... els)
  (cond [(int->bool:impl cnd) thn]
        ...
        [else els]))

(define (if:impl:return-type . ts)
  (match ts
    [(list tc tt te ...)
     (match tc
       ; If the first condition is static and true, return the type of the first "then" clause.
       [(t/static-data v _)
        (if (int->bool:impl v)
          tt
          (apply if:impl:return-type te))]
       ; If the first condition is not static, return a union of the type of the first "then" clause
       ; and the type of the rest.
       [_
        (t/union (list tt (apply if:impl:return-type te)))])]
    [(list te) te]))

; The Hydromel case statement is expanded to a call-expr to _case_.
(define-syntax _case_ (meta/make-function #'case:impl))

(define-syntax-parse-rule (case:impl expr (~seq ch thn) ... (~optional els))
  #:with els^ (or (attribute els) #'(error "Value did not not match any choice"))
  (let ([v expr])
    (cond [(member v ch) thn]
          ...
          [else els^])))

(define (case:impl:return-type tx . ts)
  (match tx
    ; If the expression is static and true, inspect the cases for static choices.
    [(t/static-data v _)
     (apply case:impl:return-type/static v ts)]
    ; If the expression is not static, return a union of all target clauses.
    [_
     (define last-n (sub1 (length ts)))
     (t/union (for/list ([it (in-list ts)]
                         [n (in-naturals)]
                         #:when (or (odd? n) (= n last-n)))
                it))]))

(define (case:impl:return-type/static v . ts)
  (match ts
    [(list tc tt te ...)
     ; If the expression value matches a static choice, return the corresponding type.
     (define tc^ (filter t/static-data? tc))
     (if (member v (map t/static-data-value tc^))
       tt
       (apply case:impl:return-type/static v te))]
    [(list te) te]))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/make-function #'min-unsigned-width))

(define-return-type (min-unsigned-width ta)
  (~> ta
      t/normalize-type
      t/abstract-integer-width
      t/unsigned))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/make-function #'min-signed-width))

(define-return-type (min-signed-width ta)
  (match (t/normalize-type ta)
    [(t/signed   n) (t/unsigned n)]
    [(t/unsigned n) (t/unsigned (add1 n))]
    [_ (error "Cannot compute data size.")]))

; Boolean operators are all bitwise.
(define-syntax _not_ (meta/make-function/cast #'bitwise-not))
(define-syntax _and_ (meta/make-function      #'bitwise-and))
(define-syntax _or_  (meta/make-function      #'bitwise-ior))
(define-syntax _xor_ (meta/make-function      #'bitwise-xor))

(define (bitwise:return-type ta tb)
  (match (list (t/normalize-type ta) (t/normalize-type tb))
    [(list (t/unsigned na)          (t/unsigned nb))          (t/unsigned (max na nb))]
    [(list (t/signed   na)          (t/abstract-integer  nb)) (t/signed   (max na nb))]
    [(list (t/abstract-integer  na) (t/signed   nb))          (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define bitwise-not:return-type identity)
(define-return-type (bitwise-and ta tb) (bitwise:return-type ta tb))
(define-return-type (bitwise-ior ta tb) (bitwise:return-type ta tb))
(define-return-type (bitwise-xor ta tb) (bitwise:return-type ta tb))

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

(define-return-type (eq:impl ta tb) (t/unsigned 1))
(define-return-type (ne:impl ta tb) (t/unsigned 1))
(define-return-type (gt:impl ta tb) (t/unsigned 1))
(define-return-type (lt:impl ta tb) (t/unsigned 1))
(define-return-type (ge:impl ta tb) (t/unsigned 1))
(define-return-type (le:impl ta tb) (t/unsigned 1))

; Use the built-in arithmetic operators.
(define-syntax _+_   (meta/make-function #'+))
(define-syntax _-_   (meta/make-function #'-))
(define-syntax _*_   (meta/make-function #'*))
(define-syntax _/_   (meta/make-function #'quotient))
(define-syntax _neg_ (meta/make-function #'neg:impl))

(define (+-:return-type ta tb)
  (define tr (t/common-supertype/normalize ta tb))
  (match tr
    [(t/abstract-integer w) (t/resize tr (add1 w))]
    [_ (error "Arithmetic operation expects integer operands." ta tb)]))

(define-return-type (+ ta tb) (+-:return-type ta tb))
(define-return-type (- ta tb) (+-:return-type ta tb))

(define (neg:impl a)
  (- a))

(define-return-type (neg:impl ta)
  (~> ta
      t/normalize-type
      t/abstract-integer-width
      add1
      t/signed))

(define-return-type (* ta tb)
  (match (list (t/normalize-type ta) (t/normalize-type tb))
    [(list (t/unsigned na)         (t/unsigned nb))          (t/unsigned (+ na nb))]
    [(list (t/abstract-integer na) (t/signed   nb))          (t/signed   (+ na nb))]
    [(list (t/signed na)           (t/abstract-integer  nb)) (t/signed   (+ na nb))]
    [_ (error "Arithmetic operation expects integer operands.")]))

(define-return-type (quotient ta tb)
  ta)

(define-syntax _<<_ (meta/make-function #'arithmetic-shift))
(define-syntax _>>_ (meta/make-function #'arithmetic-shift-right))

(define-return-type (arithmetic-shift ta tb)
  (define ta^ (t/normalize-type ta))
  (match (list ta^ tb)
    [(list (t/abstract-integer na) (t/static-data nb _)) (t/resize ta^ (max 0 (+ na nb)))]
    [(list (t/abstract-integer na) (t/unsigned nb))      (t/resize ta^ (+ na (max-unsigned-value nb)))]
    [(list (t/abstract-integer na) (t/signed nb))        (t/resize ta^ (+ na (max-signed-value nb)))]
    [_ (error "Shift operation expects integer operands.")]))

(define (arithmetic-shift-right a b)
  (arithmetic-shift a (- b)))

(define-return-type (arithmetic-shift-right ta tb)
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

(define-return-type (range:impl ta tb)
  (define tr (t/common-supertype/normalize ta tb))
  (unless (t/abstract-integer? tr)
    (error "Range expects integer boundaries."))
  (t/range tr))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
(define-syntax _slice_ (meta/make-function/cast #'unsigned-slice))

(define-return-type (unsigned-slice ta tb tc)
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

(define-return-type (concat:impl . ts)
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

(define-return-type (pvector . ts)
  (t/array (length ts) (t/union ts)))

(define-syntax _record_ (meta/make-function #'hash))

(define-return-type (hash . ts)
  (define ts^ (for/list ([it (in-list ts)]
                         [n  (in-naturals)])
                (if (even? n)
                  (t/static-data-value it)
                  it)))
  (t/record (apply hash ts^)))

(define-syntax _nth_ (meta/make-function #'nth))

(define-return-type (nth ta tb)
  ; TODO check the type of tb
  (match (t/normalize-type ta)
    [(t/array _ te) te]
    [_              (error "Not an array type" ta)]))

(define-syntax _set_nth_ (meta/make-function #'set-nth))

(define-return-type (set-nth ta tb tc)
  ; TODO check the types of tb and tc
  ta)

(define-syntax _field_ (meta/make-function #'dict-ref))

(define-return-type (dict-ref ta tb)
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

(define-return-type (cast:impl ta tb)
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
