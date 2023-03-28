; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "function.rkt"
  "types.rkt"
  "errors.rkt"
  (only-in "numeric.rkt"
    min-unsigned-width min-signed-width
    min-signed-value   max-signed-value
    min-unsigned-value max-unsigned-value
    unsigned-slice     set-slice
    unsigned-concat*)
  syntax/parse/define
  threading
  (only-in data/collection
    nth set-nth)
  data/pvector)

; ------------------------------------------------------------------------------
; Common types.
; ------------------------------------------------------------------------------

(define any*     (any-type))
(define type*    (subtype-type any*))
(define integer* (signed-type #f))
(define natural* (unsigned-type #f))
(define record*  (record-type (hash)))
(define array*   (array-type 0 any*))
(define symbol*  (symbol-type #f))
(define bit*     (unsigned-type 1))
(define boolean* (boolean-type))
(define union*   (union-type empty))

; ------------------------------------------------------------------------------
; Conditionals.
; ------------------------------------------------------------------------------

; Convert an integer to a Racket boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-function int->bool
  (λ (a)
    (not (zero? a)))
  (λ (ta)
    (assert-<: 0 ta integer*)
    boolean*))

; The Hydromel if statement is implemented as the _if_ macro
; that expands to `cond`.
(declare-function _if_)

(define-syntax-parse-rule (_if_ (~seq cnd thn) ... els)
  (cond [(int->bool cnd) thn]
        ...
        [else els]))

(define-function/return-type _if_ _if_/return-type)

(define (_if_/return-type . ts)
  (match ts
    [(list tc tt te ...)
     (match tc
       ; If the first condition is static and true, return the type of the first "then" clause.
       [(const-type v _)
        (if (int->bool v)
          tt
          (apply _if_/return-type te))]
       ; If the first condition is not static, return a union of the type of the first "then" clause
       ; and the type of the rest.
       [_
        (union-type (list tt (apply _if_/return-type te)))])]
    [(list te) te]))

; The Hydromel case statement is implemented as the _case_ macro
; that expands to `cond`.
(declare-function _case_)

(define-syntax-parse-rule (_case_ expr (~seq ch thn) ... (~optional els))
  ; TODO raise runtime error with source information
  #:with els^ (or (attribute els) #'(error "Value did not not match any choice"))
  (let ([v expr])
    (cond [(member v ch) thn]
          ...
          [else els^])))

(define-function/return-type _case_
  (λ (tx . ts)
    (match tx
      ; If the expression is static, inspect the cases for static choices.
      [(const-type v _)
       (_case_/return-type v ts)]
      ; If the expression is not static, return a union of all target clauses.
      [_
       (define last-n (sub1 (length ts)))
       (union-type (for/list ([(t n) (in-indexed ts)]
                              #:when (or (odd? n) (= n last-n)))
                     t))])))

(define (_case_/return-type v ts)
  (match ts
    [(list tc tt te ...)
     ; If the expression value matches a static choice, return the corresponding type.
     (define tc^ (filter const-type? (tuple-type-elt-types tc)))
     (if (member v (map const-type-value tc^))
       tt
       (_case_/return-type v te))]
    [(list te) te]
    ['() (raise-semantic-error "Value did not match any choice" (current-typecheck-stx) 0)]))

; ------------------------------------------------------------------------------
; Boolean and bitwise operations.
; Boolean operators are all bitwise.
; ------------------------------------------------------------------------------

; Operator `not` maps to the Racket function `bitwise-not`.
; It needs casting to preserve the width and signedness of the operand.
(define-function/cast _not_
  bitwise-not
  (λ (t)
    (assert-<: 0 t integer*)
    t))

; Operator `and` maps to the Racket function `bitwise-and`.
; When both operands are signed, the result is signed.
; When at least one operand is unsigned, the result is unsigned.
; The width of the result is the width of the widest operand.
(define-function _and_
  bitwise-and
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (match* ((minimize ta) (minimize tb))
      [((signed-type            na) (signed-type           nb)) (signed-type   (max na nb))]
      [((unsigned-type          na) (abstract-integer-type nb)) (unsigned-type (max na nb))]
      [((abstract-integer-type  na) (unsigned-type         nb)) (unsigned-type (max na nb))])))

; Operator `or` maps to the Racket function `bitwise-ior`.
; When both operands are unsigned, the result is unsigned.
; When at least one operand is signed, the result is signed.
; The width of the result is the width of the widest operand.
(define-function _or_
  bitwise-ior
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (match* ((minimize ta) (minimize tb))
      [((unsigned-type         na) (unsigned-type          nb)) (unsigned-type (max na nb))]
      [((signed-type           na) (abstract-integer-type  nb)) (signed-type   (max na nb))]
      [((abstract-integer-type na) (signed-type            nb)) (signed-type   (max na nb))])))

; Operator `xor` maps to the Racket function `bitwise-xor`.
; When both operands are unsigned, the result is unsigned.
; When at least one operand is signed, the result is signed.
; The width of the result is computed to preserve all bits that could be
; affected by the operation.
(define-function _xor_
  bitwise-xor
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (match* ((minimize ta) (minimize tb))
      [((unsigned-type na) (unsigned-type nb)) (unsigned-type (max na nb))]
      [((signed-type   na) (unsigned-type nb)) (signed-type   (max na (add1 nb)))]
      [((unsigned-type na) (signed-type   nb)) (signed-type   (max (add1 na) nb))]
      [((signed-type   na) (signed-type   nb)) (signed-type   (max na nb))])))

; ------------------------------------------------------------------------------
; Arithmetic operations.
; ------------------------------------------------------------------------------

; Returns the minimum width to encode a given number as an unsigned integer.
; See function `min-unsigned-width` in numeric.rkt.
(define-function unsigned_width
  min-unsigned-width
  (λ (t)
    (assert-<: 0 t integer*)
    (~>> t
         minimize
         abstract-integer-type-width
         type-of)))

; Returns the minimum width to encode a given number as an signed integer.
; See function `min-signed-width` in numeric.rkt.
(define-function signed_width
  min-signed-width
  (λ (t)
    (assert-<: 0 t integer*)
    (match (minimize t)
      [(signed-type   n) (type-of n)]
      [(unsigned-type n) (type-of (add1 n))])))

; The `==` operator compares its operand for equality.
; Operands are not typechecked.
(define-function _==_
  (λ (a b)
    (if (equal? a b) 1 0))
  (const bit*))

; The `/=` operator compares its operand for inequality.
; Operands are not typechecked.
(define-function _/=_
  (λ (a b)
    (if (equal? a b) 0 1))
  (const bit*))

; Other comparison operators expect integer operands and return a bit.
(define (comparison-return-type ta tb)
  (assert-<: 0 ta integer* tb integer*)
  bit*)

(define-function _>_
  (λ (a b)
    (if (> a b) 1 0))
  comparison-return-type)

(define-function _<_
  (λ (a b)
    (if (< a b) 1 0))
  comparison-return-type)

(define-function _>=_
  (λ (a b)
    (if (>= a b) 1 0))
  comparison-return-type)

(define-function _<=_
  (λ (a b)
    (if (<= a b) 1 0))
  comparison-return-type)

; Addition and subtraction expect integer operands.
; To compute the result type, we determine the common supertype of the operands
; and we resize it to add the carry bit.
(define (add-sub-return-type ta tb)
  (assert-<: 0 ta integer* tb integer*)
  (define tr (minimize (common-supertype ta tb)))
  (resize tr (add1 (abstract-integer-type-width tr))))

; Operators `+` and `-` map to the corresponding Racket functions.
(define-function _+_ + add-sub-return-type)
(define-function _-_ - add-sub-return-type)

; Operator `*` maps to the Racket operator `*`.
; When both operands are unsigned, the result is unsigned.
; When at least one operand is signed, the result is signed.
; The result width is the sum of the operand widths.
(define-function _*_
  *
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (match* ((minimize ta) (minimize tb))
      [((unsigned-type         na) (unsigned-type         nb)) (unsigned-type (+ na nb))]
      [((abstract-integer-type na) (signed-type           nb)) (signed-type   (+ na nb))]
      [((signed-type           na) (abstract-integer-type nb)) (signed-type   (+ na nb))])))

; Operator `/` maps to the Racket function `quotient`.
; When the divisor is unsigned, the result has the same type as the dividend.
; When the divisor is signed, the result is signed.
(define-function _/_
  quotient
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (match* ((minimize ta) (minimize tb))
      [(ta^                        (unsigned-type _)) ta^]
      [((abstract-integer-type na) (signed-type   _)) (signed-type (add1 na))])))

; Unary operator `-` maps to the Racket function `-` applied to one operand.
; The result is always signed and is one bit wider than the operand.
(define-function _neg_
  -
  (λ (t)
    (assert-<: 0 t integer*)
    (~>> t
         minimize
         abstract-integer-type-width
         add1
         signed-type)))

; The operator `<<` maps to the racket function `arithmetic-shift`.
; The result has the same signedness as the left operand.
; The width of the result can be determined if the right operand is constant,
; otherwise, we consider the worst case.
(define-function _<<_
  arithmetic-shift
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (define ta^ (minimize ta))
    (define na (abstract-integer-type-width ta^))
    (match tb
      [(const-type nb _)  (resize ta^ (max 0 (+ na nb)))]
      [(unsigned-type nb) (resize ta^ (+ na (max-unsigned-value nb)))]
      [(signed-type nb)   (resize ta^ (+ na (max-signed-value nb)))])))

; The operator `<<` maps to the racket function `arithmetic-shift`, taking
; the opposite of the right operand.
; The result has the same signedness as the left operand.
; The width of the result can be determined if the right operand is constant,
; otherwise, we consider the worst case.
(define-function _>>_
  (λ (a b)
    (arithmetic-shift a (- b)))
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (define ta^ (minimize ta))
    (define na (abstract-integer-type-width ta^))
    (match tb
      [(const-type    nb _) (resize ta^ (max 0 (- na nb)))]
      [(unsigned-type nb)   ta^]
      [(signed-type   nb)   (resize ta^ (- na (min-signed-value nb)))])))

; The operator `..` is implemented by the `_range_` function.
; It expects two integer operands.
; The result has type `range-type`.
; TODO Empty ranges are not supported.
; TODO Do we need an explicit "descending" range specifier?
(define-function _range_
  (λ (a b)
    (if (<= a b)
      (range a (add1 b))
      (range a (sub1 b) -1)))
  (λ (ta tb)
    (assert-<: 0 ta integer* tb integer*)
    (range-type (common-supertype ta tb))))

; The slicing operation maps to the `unsigned-slice` function from module
; numeric.rkt. The function below needs explicit casting to handle the signed
; case automatically.
(define-function/cast _slice_
  unsigned-slice
  (λ (tx tl tr)
    (assert-<: 0 tx integer* tl integer* tr integer*)
    (define left (match tl
                   [(const-type    v _) v]
                   [(unsigned-type n)   (max-unsigned-value n)]
                   [(signed-type   n)   (max-signed-value   n)]))
    (define right (match tr
                   [(const-type    v _) v]
                   [(unsigned-type n)   (min-unsigned-value n)]
                   [(signed-type   n)   (min-signed-value   n)]))
    (resize (minimize tx) (max 0 (add1 (- left right))))))

; The slice assignment operator is implemented using the `set-slice` function
; from numeric.rkt. Its return type is the type of the first operand.
(define-function _set_slice_
  (λ (x . lrvs)
    (let loop ([res x] [rst lrvs])
      (match rst
        [(list l r v rst^ ...) (loop (set-slice res l r v) rst^)]
        [_                      res])))
  (λ ts
    (for ([(t pos) (in-indexed ts)])
      (assert-<: pos t integer*))
    (first ts)))

; The binary concatenetion operation is based on the function `unsigned-concat*`
; from module numeric.rkt. The function below needs explicit casting to handle
; the signed case automatically.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define-function/cast _concat_
  (λ vts
    (unsigned-concat*
      (let loop ([res empty] [rst vts])
        (match rst
          [(list rst^ ... v t)
           (define l (~>> t
                          minimize
                          abstract-integer-type-width
                          sub1))
           (loop (cons (list v l 0) res) rst^)]
          [_ res]))))
  (λ ts
    ; Check that arguments at even positions have integer types.
    (define ts^ (for/list ([(t pos) (in-indexed ts)] #:when (even? pos))
                  (assert-<: pos t integer*)
                  (minimize t)))
    ; Compute the total width of the result.
    (define w (for/sum ([(t pos) (in-indexed ts^)])
                (or (abstract-integer-type-width t)
                    (raise-semantic-error "Operand width could not be determined" (current-typecheck-stx) (* 2 pos)))))
    ; The result has the same signedness as the first argument.
    ; If there is no argument, `_concat_` retuns bit 0.
    (match ts^
      [(list (signed-type   _) _ ...) (signed-type   w)]
      [(list (unsigned-type _) _ ...) (unsigned-type w)]
      [_                              bit*])))

; ------------------------------------------------------------------------------
; Array and record operations.
; ------------------------------------------------------------------------------

; The array constructor creates a persistent vector.
(define-function _array_
  pvector
  (λ ts
    (array-type (length ts) (union-type ts))))

; Array access maps to the `_nth_` function.
; This function handles the case of multidimensional access.
(define-function _nth_
  (λ (a . ns)
    (for/fold ([res a])
              ([n (in-list ns)])
      (nth res n)))
  (λ (ta . tns)
    (for/fold ([res ta])
              ([(t pos) (in-indexed tns)])
      (assert-<: pos res array* t integer*)
      (array-type-elt-type (minimize res)))))

; Array assignment maps to the `_set_nth_` function.
; It handles setting one or more values to one or more indices.
; It delegates to set-nth/multi to handle the multidimensional case.
(define-function _set_nth_
  (λ (a . nvs)
    (let loop ([res a] [rst nvs])
      (match rst
        [(list n v rst^ ...) (loop (set-nth/multi res n v) rst^)]
        [_                   res])))
  (λ (ta . tnvs)
    (assert-<: 0 ta array*)
    (define ta^ (minimize ta))
    (let loop ([rst tnvs] [pos 1])
      (unless (empty? rst)
        (match-define (list tn tv rst^ ...) rst)
        (define te (match (minimize tn)
                     [(tuple-type (list tns ...))
                      (for/fold ([te ta^])
                                ([t (in-list tns)])
                        (assert-<: pos t integer*)
                        (array-type-elt-type te))]
                     [t
                      (assert-<: pos t integer*)
                      (array-type-elt-type ta^)]))
        (assert-<: (add1 pos) tv te)
        (loop rst^ (+ 2 pos))))
    ta^))

; Assignment to a multidimensional array.
(define (set-nth/multi arr ns v)
  (match ns
    [(or (? number? n) (list n)) (set-nth arr n v)]
    [(list n ns^ ...)            (~> (nth arr n)
                                     (set-nth/multi _ ns^ v)
                                     (set-nth arr n _))]))

; The record constructor creates a hash table.
; Even arguments are keys. Odd arguments are values.
(define-function _record_
  hash
  (λ kts
    (~>> (for/list ([(kt pos) (in-indexed kts)])
           (if (even? pos)
             (begin
               ; Check that even arguments are symbols and extract them.
               (assert-<: pos kt symbol*)
               (const-type-value kt))
             kt))
         (apply hash)
         record-type)))

(define-function _field_
  dict-ref
  (λ (tk tv)
    (assert-<: 0 tk record* tv symbol*)
    (define fields     (~> tk minimize record-type-fields))
    (define field-name (~> tv minimize symbol-type-value))
    (dict-ref fields field-name
      (thunk (error "Unknown field" field-name)))))

(define-function _set_field_
  (λ (r . kvs)
    (let loop ([res r] [rst kvs])
      (match rst
        [(list k v rst^ ...) (loop (dict-set res k v) rst^)]
        [_                   res])))
  (λ (tr . tkvs)
    (assert-<: 0 tr record*)
    (define tr^ (minimize tr))
    (define fields (~> tr^ minimize record-type-fields))
    (let loop ([pos 1] [rst tkvs])
      (match rst
        [(list tk tv rst^ ...)
         (assert-<: pos tk symbol*)
         (define field-name (~> tk minimize symbol-type-value))
         (define tf (dict-ref fields field-name
                      (thunk (error "Unknown field" field-name))))
         (assert-<: (add1 pos) tv tf)
         (loop (+ 2 pos) rst^)]
        [_ tr^]))))

(define-function _tuple_
  list
  (λ ts
    (tuple-type ts)))

; ------------------------------------------------------------------------------
; Type operations.
; ------------------------------------------------------------------------------

; _cast_ does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(declare-function/cast _cast_)

(define-syntax-parse-rule (_cast_ a b) b)

(define-function/return-type _cast_
  (λ (ta tb)
    (assert-<: 0 ta type*)
    (define ta^ (minimize (const-type-value ta)))
    (define tr (match ta^
                 [(signed-type #f)
                  (assert-<: 1 tb integer*)
                  (define tb^ (minimize tb))
                  (match tb^
                    [(signed-type   _) tb^]
                    [(unsigned-type n) (signed-type n)])]

                 [(unsigned-type #f)
                  (assert-<: 1 tb integer*)
                  (define tb^ (minimize tb))
                  (match tb^
                    [(signed-type   n) (unsigned-type n)]
                    [(unsigned-type _) tb^])]

                 [_
                  ; TODO Add checks here.
                  ta^]))
    ; Enforce the type of the result, even if it is a static value.
    (match tb
      [(const-type v _) (const-type (tr v) tr)]
      [_                tr])))

(define-function zero
  (λ (t)
    (match (minimize t)
      [(abstract-integer-type _) 0]
      [(array-type n v)          (make-pvector n (zero v))]
      [(tuple-type ts)           (map zero ts)]
      [(record-type fs)          (for/hash ([(k v) (in-dict fs)])
                                   (values k (zero v)))]
      [(union-type ts)           (zero (first ts))]
      [(symbol-type s)           s]
      [_ (error "This type does not support a zero value" t)]))
  (λ (t)
    (assert-<: 0 t type*)
    (const-type-value t)))

; ------------------------------------------------------------------------------
; Type constructors.
; ------------------------------------------------------------------------------

(define-function any any-type
  (const type*))

(define-function none
  (const union*)
  (const type*))

(define-function signed signed-type
  (λ (t)
    (assert-const 0 t)
    (assert-<:    0 t integer*)
    type*))

(define-function unsigned unsigned-type
  (λ (t)
    (assert-const 0 t)
    (assert-<:    0 t integer*)
    type*))

(define-function tuple
  (λ args
    (tuple-type args))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const n t)
      (assert-<:    n t type*))
    type*))

(define-function union
  (λ args
    (union-type args))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const n t)
      (assert-<:    n t type*))
    type*))

(define-function array
  (λ args
    (match args
      [(list t) t]
      [(list n nts ...) (array-type n (apply array nts))]))
  (λ ts
    (define last-n (sub1 (length ts)))
    (for ([(t n) (in-indexed ts)])
      (assert-const n t)
      (assert-<:    n t (if (= n last-n) type* integer*)))
    type*))

(define-function record
  (λ args
    (record-type (apply hash args)))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const n t)
      (assert-<:    n t (if (even? n) symbol* type*)))
    type*))

(define-function type
  (const type*)
  (const type*))

(define-function subtype subtype-type
  (const type*))

(define-function bit
  (const bit*)
  (const type*))

(define-function natural
  (const natural*)
  (const type*))

(define-function integer
  (const integer*)
  (const type*))

(define-function enumeration
  (λ args
    (union-type (map symbol-type args)))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const n t)
      (assert-<:    n t symbol*))
    type*))
