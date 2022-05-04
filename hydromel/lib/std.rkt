; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "function.rkt"
  "types.rkt"
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

(define (assert-<: name pos t u)
  (unless (<: t u)
    (raise-argument-error name (type->string u) pos (type->string t))))

(define (assert-const name pos t)
  (unless (const-type? t)
    (raise-argument-error name "constant value" pos (type->string t))))

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
    (assert-<: 'int->bool 0 ta (integer))
    (boolean-type)))

; The Hydromel if statement is expanded to a call-expr to _if_.
(declare-function _if_)

(define-syntax-parse-rule (_if_ (~seq cnd thn) ... els)
  (cond [(int->bool cnd) thn]
        ...
        [else els]))

(define (_if_:return-type . ts)
  (match ts
    [(list tc tt te ...)
     (match tc
       ; If the first condition is static and true, return the type of the first "then" clause.
       [(const-type v _)
        (if (int->bool v)
          tt
          (apply _if_:return-type te))]
       ; If the first condition is not static, return a union of the type of the first "then" clause
       ; and the type of the rest.
       [_
        (union-type (list tt (apply _if_:return-type te)))])]
    [(list te) te]))

; The Hydromel case statement is expanded to a call-expr to _case_.
(declare-function _case_)

(define-syntax-parse-rule (_case_ expr (~seq ch thn) ... (~optional els))
  #:with els^ (or (attribute els) #'(error "Value did not not match any choice"))
  (let ([v expr])
    (cond [(member v ch) thn]
          ...
          [else els^])))

(define (_case_:return-type tx . ts)
  (match tx
    ; If the expression is static and true, inspect the cases for static choices.
    [(const-type v _)
     (apply _case_:return-type/static v ts)]
    ; If the expression is not static, return a union of all target clauses.
    [_
     (define last-n (sub1 (length ts)))
     (union-type (for/list ([(it n) (in-indexed ts)]
                            #:when (or (odd? n) (= n last-n)))
                   it))]))

(define (_case_:return-type/static v . ts)
  (match ts
    [(list tc tt te ...)
     ; If the expression value matches a static choice, return the corresponding type.
     (define tc^ (filter const-type? (tuple-type-elt-types tc)))
     (if (member v (map const-type-value tc^))
       tt
       (apply _case_:return-type/static v te))]
    [(list te) te]))

; ------------------------------------------------------------------------------
; Boolean and bitwise operations.
;
; Boolean operators are all bitwise.
; ------------------------------------------------------------------------------

(define-function/cast _not_ bitwise-not
  (λ (t)
    (assert-<: 'not 0 t (integer))
    t))

(define-function _and_ bitwise-and
  (λ (ta tb)
    (assert-<: 'and 0 ta (integer))
    (assert-<: 'and 1 tb (integer))
    (match (list (minimize ta) (minimize tb))
      [(list (signed-type            na) (signed-type           nb)) (signed-type   (max na nb))]
      [(list (unsigned-type          na) (abstract-integer-type nb)) (unsigned-type (max na nb))]
      [(list (abstract-integer-type  na) (unsigned-type         nb)) (unsigned-type (max na nb))])))

(define-function _or_  bitwise-ior
  (λ (ta tb)
    (assert-<: 'or 0 ta (integer))
    (assert-<: 'or 1 tb (integer))
    (match (list (minimize ta) (minimize tb))
      [(list (unsigned-type         na) (unsigned-type          nb)) (unsigned-type (max na nb))]
      [(list (signed-type           na) (abstract-integer-type  nb)) (signed-type   (max na nb))]
      [(list (abstract-integer-type na) (signed-type            nb)) (signed-type   (max na nb))])))

(define-function _xor_ bitwise-xor
  (λ (ta tb)
    (assert-<: 'xor 0 ta (integer))
    (assert-<: 'xor 1 tb (integer))
    (match (list (minimize ta) (minimize tb))
      [(list (unsigned-type na) (unsigned-type nb)) (unsigned-type (max na nb))]
      [(list (signed-type   na) (unsigned-type nb)) (signed-type   (max na (add1 nb)))]
      [(list (unsigned-type na) (signed-type   nb)) (signed-type   (max (add1 na) nb))]
      [(list (signed-type   na) (signed-type   nb)) (signed-type   (max na nb))])))

; ------------------------------------------------------------------------------
; Arithmetic operations.
; ------------------------------------------------------------------------------

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-function unsigned_width min-unsigned-width
  (λ (t)
    (assert-<: 'unsigned_width 0 t (integer))
    (~>> t
         minimize
         abstract-integer-type-width
         type-of)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-function signed_width min-signed-width
  (λ (t)
    (assert-<: 'signed_width 0 t (integer))
    (match (minimize t)
      [(signed-type   n) (type-of n)]
      [(unsigned-type n) (type-of (add1 n))])))

; Comparison operations return integers 0 and 1.
(define-function _==_
  (λ (a b) (if (equal? a b) 1 0))
  (const (unsigned-type 1)))

(define-function _/=_
  (λ (a b) (if (equal? a b) 0 1))
  (const (unsigned-type 1)))

(define-function _>_
  (λ (a b) (if (> a b) 1 0))
  (λ (ta tb) (comparison-return-type '> ta tb)))

(define-function _<_
  (λ (a b) (if (< a b) 1 0))
  (λ (ta tb) (comparison-return-type '< ta tb)))

(define-function _>=_
  (λ (a b) (if (>= a b) 1 0))
  (λ (ta tb) (comparison-return-type '>= ta tb)))

(define-function _<=_
  (λ (a b) (if (<= a b) 1 0))
  (λ (ta tb) (comparison-return-type '<= ta tb)))

(define (comparison-return-type name ta tb)
  (assert-<: name 0 ta (integer))
  (assert-<: name 1 tb (integer))
  (unsigned-type 1))

; Use the built-in arithmetic operators.
(define-function _+_ +
  (λ (ta tb) (add-sub-return-type '+ ta tb)))

(define-function _-_ -
  (λ (ta tb) (add-sub-return-type '- ta tb)))

(define (add-sub-return-type name ta tb)
  (assert-<: name 0 ta (integer))
  (assert-<: name 1 tb (integer))
  (define tr (minimize (common-supertype ta tb)))
  (resize tr (add1 (abstract-integer-type-width tr))))

(define-function _*_ *
  (λ (ta tb)
    (assert-<: '* 0 ta (integer))
    (assert-<: '* 1 tb (integer))
    (match (list (minimize ta) (minimize tb))
      [(list (unsigned-type         na) (unsigned-type         nb)) (unsigned-type (+ na nb))]
      [(list (abstract-integer-type na) (signed-type           nb)) (signed-type   (+ na nb))]
      [(list (signed-type           na) (abstract-integer-type nb)) (signed-type   (+ na nb))])))

(define-function _/_ quotient
  (λ (ta tb)
    (assert-<: '/ 0 ta (integer))
    (assert-<: '/ 1 tb (integer))
    (match (list (minimize ta) (minimize tb))
      [(list ta^                        (unsigned-type _)) ta^]
      [(list (abstract-integer-type na) (signed-type   _)) (signed-type (add1 na))])))

(define-function _neg_
  (λ (a) (- a))
  (λ (ta)
    (assert-<: '- 0 ta (integer))
    (~>> ta
         minimize
         abstract-integer-type-width
         add1
         signed-type)))

(define-function _<<_ arithmetic-shift
  (λ (ta tb)
    (assert-<: '<< 0 ta (integer))
    (assert-<: '<< 1 tb (integer))
    (define ta^ (minimize ta))
    (define na (abstract-integer-type-width ta^))
    (match tb
      [(const-type nb _)  (resize ta^ (max 0 (+ na nb)))]
      [(unsigned-type nb) (resize ta^ (+ na (max-unsigned-value nb)))]
      [(signed-type nb)   (resize ta^ (+ na (max-signed-value nb)))])))

(define-function _>>_
  (λ (a b)
    (arithmetic-shift a (- b)))
  (λ (ta tb)
    (assert-<: '>> 0 ta (integer))
    (assert-<: '>> 1 tb (integer))
    (define ta^ (minimize ta))
    (define na (abstract-integer-type-width ta^))
    (match tb
      [(const-type    nb _) (resize ta^ (max 0 (- na nb)))]
      [(unsigned-type nb)   ta^]
      [(signed-type   nb)   (resize ta^ (- na (min-signed-value nb)))])))

; TODO Empty ranges are no longer supported.
; TODO Do we need an explicit "descending" range specifier?
(define-function _range_
  (λ (a b)
    (if (<= a b)
      (range a (add1 b))
      (range a (sub1 b) -1)))
  (λ (ta tb)
    (assert-<: 'range 0 ta (integer))
    (assert-<: 'range 1 tb (integer))
    (range-type (common-supertype ta tb))))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
(define-function/cast _slice_ unsigned-slice
  (λ (ta tb tc)
    (assert-<: 'slice 0 ta (integer))
    (assert-<: 'slice 1 tb (integer))
    (assert-<: 'slice 2 tc (integer))
    (define left (match tb
                   [(const-type n _) n]
                   [(unsigned-type   n) (max-unsigned-value n)]
                   [(signed-type     n) (max-signed-value   n)]))
    (define right (match tc
                   [(const-type n _) n]
                   [(unsigned-type   n) (min-unsigned-value n)]
                   [(signed-type     n) (min-signed-value   n)]))
    (resize (minimize ta) (max 0 (add1 (- left right))))))

(define-function _set_slice_
  (λ args
    (let loop ([res (first args)] [lrvs (rest args)])
      (match lrvs
        [(list l r v xs ...) (loop (set-slice res l r v) xs)]
        [_                   res])))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-<: 'set_slice n t (integer)))
    (first ts)))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by concat:return-type.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define-function/cast _concat_
  (λ vs
    (apply unsigned-concat*
      (let loop ([res empty] [lst vs])
        (match lst
          [(list h ... v t)
           (assert-<: 'concat (length res) t (integer))
           (define l (~>> t
                          minimize
                          abstract-integer-type-width
                          sub1))
           (loop (cons (list v l 0) res) h)]
          [_ res]))))
  (λ ts
    (define ts^ (for/list ([(t n) (in-indexed ts)] #:when (odd? n))
                  (define t^ (const-type-value t))
                  (assert-<: 'concat (/ (sub1 n) 2) t^ (integer))
                  (minimize t^)))
    (define w (for/sum ([it (in-list ts^)])
                (abstract-integer-type-width it)))
    (match ts^
      [(list (signed-type   _) _ ...) (signed-type   w)]
      [(list (unsigned-type _) _ ...) (unsigned-type w)]
      [_                              (unsigned-type 1)])))

; ------------------------------------------------------------------------------
; Array and record operations.
; ------------------------------------------------------------------------------

(define-function _array_ pvector
  (λ ts (array-type (length ts) (union-type ts))))

(define-function _record_ hash
  (λ ts
    (define ts^ (for/list ([(it n) (in-indexed ts)])
                  (if (even? n)
                    (const-type-value it)
                    it)))
    (record-type (apply hash ts^))))

(define-function _nth_
  (λ args
    (for/fold ([res (first args)])
              ([n (in-list (rest args))])
      (nth res n)))
  (λ ts
    (for/fold ([res (first ts)])
              ([(t n) (in-indexed (rest ts))])
      (assert-<: 'nth n res (array 0 (any)))
      (assert-<: 'nth (add1 n) t (integer))
      (array-type-elt-type (minimize res)))))

(define (set-nth/multi arr ns v)
  (match ns
    [(or (? number? n) (list n)) (set-nth arr n v)]
    [(list n m ...)              (set-nth arr n (set-nth/multi (nth arr n) m v))]))

(define-function _set_nth_
  (λ args
    (let loop ([res (first args)] [nvs (rest args)])
      (match nvs
        [(list n v xs ...) (loop (set-nth/multi res n v) xs)]
        [_                 res])))
  (λ ts
    (assert-<: 'set_nth 0 (first ts) (array 0 (any)))
    (define ta (minimize (first ts)))
    (let loop ([ts (rest ts)] [n 1])
      (unless (empty? ts)
        (match-define (list tn tv txs ...) ts)
        (define te (match tn
                     [(tuple-type (list tns ...))
                      (for/fold ([te ta])
                                ([t (in-list tns)])
                        (assert-<: 'set_nth n t (integer))
                        (array-type-elt-type te))]
                     [t
                      (assert-<: 'set_nth n t (integer))
                      (array-type-elt-type ta)]))
        (assert-<: 'set_nth n tv te)
        (loop txs (add1 n))))
    ta))

(define-function _field_ dict-ref
  (λ (ta tb)
    (assert-<: 'field 0 ta (record))
    (assert-<: 'field 1 tb (symbol-type #f))
    (define ta^ (minimize ta))
    (define tb^ (minimize tb))
    (define field-name (symbol-type-value tb^))
    (dict-ref (record-type-fields ta^) field-name
      (thunk (error "Unknown field" field-name)))))

(define-function _set_field_
  (λ args
    (let loop ([res (first args)] [kvs (rest args)])
      (match kvs
        [(list k v r ...) (loop (dict-set res k v) r)]
        [_                res])))
  (λ ts
    (assert-<: 'set_field 0 (first ts) (record))
    (define ta (minimize (first ts)))
    (let loop ([n 1] [kvs (rest ts)])
      (match kvs
        [(list tk tv tr ...)
         (assert-<: 'set_field 1 tk (symbol-type #f))
         (define field-name (symbol-type-value (minimize tk)))
         (define tf (dict-ref (record-type-fields ta) field-name
                      (thunk (error "Unknown field" field-name))))
         (assert-<: 'set_field (add1 n) tv tf)
         (loop (+ 2 n) tr)]
        [_ ta]))))

(define-function _tuple_ list
  (λ ts
    (tuple-type ts)))

; ------------------------------------------------------------------------------
; Type operations.
; ------------------------------------------------------------------------------

; _cast_ does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(declare-function/cast _cast_)

(define-syntax-parse-rule (_cast_ a b) b)

(define (_cast_:return-type ta tb)
  (assert-<: 'cast 0 ta (type))
  (define ta^ (minimize (const-type-value ta)))
  (define tr (match ta^
               [(signed-type #f)
                (assert-<: 'cast 1 tb (integer))
                (define tb^ (minimize tb))
                (match tb^
                  [(signed-type   _) tb^]
                  [(unsigned-type n) (signed-type n)])]

               [(unsigned-type #f)
                (assert-<: 'cast 1 tb (integer))
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
    [_                tr]))

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
    (assert-<: 'zero 0 t (type))
    (const-type-value t)))

; ------------------------------------------------------------------------------
; Type constructors.
; ------------------------------------------------------------------------------

(define-function any any-type (const (type)))

(define-function none (const (union-type empty)) (const (type)))

(define-function signed signed-type
  (λ (t)
    (assert-const 'signed 0 t)
    (assert-<:    'signed 0 t (integer))
    (type)))

(define-function unsigned unsigned-type
  (λ (t)
    (assert-const 'unsigned 0 t)
    (assert-<:    'signed   0 t (integer))
    (type)))

(define-function tuple
  (λ args
    (tuple-type args))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const 'tuple n t)
      (assert-<:    'tuple n t (type)))
    (type)))

(define-function union
  (λ args
    (union-type args))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const 'union n t)
      (assert-<:    'union n t (type)))
    (type)))

(define-function array
  (λ args
    (match args
      [(list t) t]
      [(list n nts ...) (array-type n (apply array nts))]))
  (λ ts
    (define last-n (sub1 (length ts)))
    (for ([(t n) (in-indexed ts)])
      (assert-const 'array n t)
      (assert-<:    'array n t (if (= n last-n) (integer) (type))))
    (type)))

(define-function record
  (λ args
    (record-type (apply hash args)))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const 'record n t)
      (assert-<:    'record n t (if (even? n) (symbol-type #f) (type))))
    (type)))

(define-function type
  (const (subtype-type (any-type)))
  (const (type)))

(define-function subtype subtype-type
  (const (type)))

(define-function bit
  (const (unsigned-type 1))
  (const (type)))

(define-function natural
  (const (unsigned-type #f))
  (const (type)))

(define-function integer
  (const (signed-type #f))
  (const (type)))

(define-function enumeration
  (λ args
    (union-type (map symbol-type args)))
  (λ ts
    (for ([(t n) (in-indexed ts)])
      (assert-const 'enumeration n t)
      (assert-<:    'enumeration n t (symbol-type #f)))
    (type)))
