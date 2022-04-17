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
    unsigned-concat)
  syntax/parse/define
  threading
  (only-in data/collection
    nth set-nth)
  data/pvector)

(provide (all-from-out "numeric.rkt"))

(define (expect-type* pred? msg name pos t)
  (define t^ (normalize t))
  (unless (pred? t^)
    (raise-argument-error name msg pos (type->string t^)))
  t^)

(define (expect-integers name . ts)
  (for/list ([(t n) (in-indexed ts)])
    (expect-integer name n t)))

(define (expect-integer name pos t)
  (expect-type* abstract-integer-type? "integer" name pos t))

(define (expect-array name pos t)
  (expect-type* array-type? "array" name pos t))

(define (expect-record name pos t)
  (expect-type* record-type? "record" name pos t))

(define (expect-symbol name pos t)
  (expect-type* symbol-type? "symbol" name pos t))

(define (expect-symbols name ts)
  (for/list ([(t n) (in-indexed ts)])
    (expect-symbol name n t)))

(define (expect-type name pos t)
  (expect-type* subtype-type? "type" name pos t))

(define (expect-types name ts)
  (for/list ([(t n) (in-indexed ts)])
    (expect-type name n t)))

(define (expect-value name pos t)
  (unless (const-type? t)
    (raise-argument-error name "static value" pos (type->string t)))
  t)

(define (expect-values name ts)
  (for/list ([(t n) (in-indexed ts)])
    (expect-value name n t)))

(define (expect-subtype name pos t u)
  (define u^ (normalize u))
  (expect-type* (λ (v) (<: v u)) "subtype" name pos t))

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
    (expect-integers 'int->bool ta)
    (boolean-type)))

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
       [(const-type v _)
        (if (int->bool:impl v)
          tt
          (apply _if_:impl:return-type te))]
       ; If the first condition is not static, return a union of the type of the first "then" clause
       ; and the type of the rest.
       [_
        (union-type (list tt (apply _if_:impl:return-type te)))])]
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
    [(const-type v _)
     (apply _case_:impl:return-type/static v ts)]
    ; If the expression is not static, return a union of all target clauses.
    [_
     (define last-n (sub1 (length ts)))
     (union-type (for/list ([(it n) (in-indexed ts)]
                            #:when (or (odd? n) (= n last-n)))
                   it))]))

(define (_case_:impl:return-type/static v . ts)
  (match ts
    [(list tc tt te ...)
     ; If the expression value matches a static choice, return the corresponding type.
     (define tc^ (filter const-type? (tuple-type-elt-types tc)))
     (if (member v (map const-type-value tc^))
       tt
       (apply _case_:impl:return-type/static v te))]
    [(list te) te]))

; ------------------------------------------------------------------------------
; Boolean and bitwise operations.
;
; Boolean operators are all bitwise.
; ------------------------------------------------------------------------------

(define-function/cast _not_ bitwise-not
  (λ (t)
    (expect-integers 'not t)
    t))

(define-function _and_ bitwise-and
  (λ (ta tb)
    (match (expect-integers 'and ta tb)
      [(list (signed-type            na) (signed-type           nb)) (signed-type   (max na nb))]
      [(list (unsigned-type          na) (abstract-integer-type nb)) (unsigned-type (max na nb))]
      [(list (abstract-integer-type  na) (unsigned-type         nb)) (unsigned-type (max na nb))])))

(define-function _or_  bitwise-ior
  (λ (ta tb)
    (match (expect-integers 'or ta tb)
      [(list (unsigned-type         na) (unsigned-type          nb)) (unsigned-type (max na nb))]
      [(list (signed-type           na) (abstract-integer-type  nb)) (signed-type   (max na nb))]
      [(list (abstract-integer-type na) (signed-type            nb)) (signed-type   (max na nb))])))

(define-function _xor_ bitwise-xor
  (λ (ta tb)
    (match (expect-integers 'xor ta tb)
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
  (λ (ta)
    (~>> ta
         (expect-integer 'unsigned_width 0)
         abstract-integer-type-width
         type-of-literal)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-function signed_width min-signed-width
  (λ (ta)
    (match (expect-integer 'signed_width 0 ta)
      [(signed-type   n) (type-of-literal n)]
      [(unsigned-type n) (type-of-literal (add1 n))])))

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
  (expect-integers name ta tb)
  (unsigned-type 1))

; Use the built-in arithmetic operators.
(define-function _+_ +
  (λ (ta tb) (add-sub-return-type '+ ta tb)))

(define-function _-_ -
  (λ (ta tb) (add-sub-return-type '- ta tb)))

(define (add-sub-return-type name ta tb)
  (define tr (apply common-supertype (expect-integers name ta tb)))
  (resize tr (add1 (abstract-integer-type-width tr))))

(define-function _*_ *
  (λ (ta tb)
    (match (expect-integers '* ta tb)
      [(list (unsigned-type         na) (unsigned-type         nb)) (unsigned-type (+ na nb))]
      [(list (abstract-integer-type na) (signed-type           nb)) (signed-type   (+ na nb))]
      [(list (signed-type           na) (abstract-integer-type nb)) (signed-type   (+ na nb))])))

(define-function _/_ quotient
  (λ (ta tb)
    (match (expect-integers '/ ta tb)
      [(list ta^                        (unsigned-type _)) ta^]
      [(list (abstract-integer-type na) (signed-type   _)) (signed-type (add1 na))])))

(define-function _neg_
  (λ (a) (- a))
  (λ (ta)
    (~>> ta
        (expect-integer '- 0)
        abstract-integer-type-width
        add1
        signed-type)))

(define-function _<<_ arithmetic-shift
  (λ (ta tb)
    (define ta^ (first (expect-integers '<< ta tb)))
    (match (list ta^ tb)
      [(list (abstract-integer-type na) (const-type nb _))  (resize ta^ (max 0 (+ na nb)))]
      [(list (abstract-integer-type na) (unsigned-type nb)) (resize ta^ (+ na (max-unsigned-value nb)))]
      [(list (abstract-integer-type na) (signed-type nb))   (resize ta^ (+ na (max-signed-value nb)))])))

(define-function _>>_
  (λ (a b)
    (arithmetic-shift a (- b)))
  (λ (ta tb)
    (define ta^ (first (expect-integers '>> ta tb)))
    (match (list ta^ tb)
      [(list (abstract-integer-type na) (const-type    nb _)) (resize ta^ (max 0 (- na nb)))]
      [(list (abstract-integer-type na) (unsigned-type nb))   ta^]
      [(list (abstract-integer-type na) (signed-type   nb))   (resize ta^ (- na (min-signed-value nb)))])))

; TODO Empty ranges are no longer supported.
; TODO Do we need an explicit "descending" range specifier?
(define-function _range_
  (λ (a b)
    (if (<= a b)
      (range a (add1 b))
      (range a (sub1 b) -1)))
  (λ (ta tb)
    (range-type (apply common-supertype (expect-integers 'range ta tb)))))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
(define-function/cast _slice_ unsigned-slice
  (λ (ta tb tc)
    (define ta^ (first (expect-integers 'slice ta tb tc)))
    (define left (match tb
                   [(const-type n _) n]
                   [(unsigned-type   n) (max-unsigned-value n)]
                   [(signed-type     n) (max-signed-value   n)]))
    (define right (match tc
                   [(const-type n _) n]
                   [(unsigned-type   n) (min-unsigned-value n)]
                   [(signed-type     n) (min-signed-value   n)]))
    (resize ta^ (max 0 (add1 (- left right))))))

(define-function _set_slice_
  (λ args
    (let loop ([res (first args)] [lrvs (rest args)])
      (match lrvs
        [(list l r v xs ...) (loop (set-slice res l r v) xs)]
        [_                   res])))
  (λ ts
    (first (for/list ([(t n) (in-indexed ts)])
             (expect-integer 'set_slice n t)))))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by concat:impl:return-type.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define-function/cast _concat_
  (λ vs
    (apply unsigned-concat
      (let loop ([res empty] [lst vs])
        (match lst
          [(list h ... v t)
           (define l (~>> t
                          (expect-integer 'concat (length res))
                          abstract-integer-type-width
                          sub1))
           (loop (cons (list v l 0) res) h)]
          [_ res]))))
  (λ ts
    (define ts^ (apply expect-integers 'concat
                  (for/list ([(t n) (in-indexed ts)] #:when (odd? n))
                    (const-type-value t))))
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
      (expect-integer 'nth (add1 n) t)
      (array-type-elt-type (expect-array 'nth n res)))))

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
    (define ta (expect-array 'set_nth 0 (first ts)))
    (let loop ([ts (rest ts)] [n 1])
      (unless (empty? ts)
        (match-define (list tn tv txs ...) ts)
        (define te (match tn
                     [(tuple-type (list tns ...))
                      (for/fold ([te ta])
                                ([t (in-list tns)])
                        (expect-integer 'set_nth n t)
                        (array-type-elt-type te))]
                     [t
                      (expect-integer 'set_nth n t)
                      (array-type-elt-type ta)]))
        (expect-subtype 'set_nth n tv te)
        (loop txs (add1 n))))
    ta))

(define-function _field_ dict-ref
  (λ (ta tb)
    (define ta^ (expect-record 'field 0 ta))
    (define tb^ (expect-symbol 'field 1 tb))
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
    (define ta (expect-record 'set_field 0 (first ts)))
    (let loop ([n 1] [kvs (rest ts)])
      (match kvs
        [(list tk tv tr ...)
         (define tk^ (expect-symbol 'set_field n tk))
         (define field-name (symbol-type-value tk^))
         (define tf (dict-ref (record-type-fields ta) field-name
                      (thunk (error "Unknown field" field-name))))
         (expect-subtype 'set_field (add1 n) tv tf)
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
(define-function*/cast _cast_)

(define-syntax-parse-rule (_cast_:impl a b) b)

(define (_cast_:impl:return-type ta tb)
  (expect-type 'cast 0 ta)
  (define ta^ (normalize (const-type-value ta)))
  (define tr (match ta^
               [(signed-type #f)
                (define tb^ (expect-integer 'cast 1 tb))
                (match tb^
                  [(signed-type   _) tb^]
                  [(unsigned-type n) (signed-type n)])]

               [(unsigned-type #f)
                (define tb^ (expect-integer 'cast 1 tb))
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
    (match (normalize t)
      [(abstract-integer-type _) 0]
      [(array-type n v)          (make-pvector n (zero:impl v))]
      [(tuple-type ts)           (map zero:impl ts)]
      [(record-type fs)          (for/hash ([(k v) (in-dict fs)])
                                   (values k (zero:impl v)))]
      [(union-type ts)           (zero:impl (first ts))]
      [(symbol-type s)           s]
      [_ (error "This type does not support a zero value" t)]))
  (λ (t)
    (expect-type 'zero 0 t)
    (const-type-value t)))

; ------------------------------------------------------------------------------
; Type constructors.
; ------------------------------------------------------------------------------

(define-function any any-type (const (type:impl)))

(define-function none none-type (const (type:impl)))

(define-function signed signed-type
  (λ (t)
    (expect-value   'signed 0 t)
    (expect-integer 'signed 0 t)
    (type:impl)))

(define-function unsigned unsigned-type
  (λ (t)
    (expect-value   'unsigned 0 t)
    (expect-integer 'unsigned 0 t)
    (type:impl)))

(define-function tuple
  (λ args
    (tuple-type args))
  (λ ts
    (expect-values 'tuple ts)
    (expect-types  'tuple ts)
    (type:impl)))

(define-function union
  (λ args
    (union-type args))
  (λ ts
    (expect-values 'union ts)
    (expect-types  'union ts)
    (type:impl)))

(define-function array
  (λ args
    (match args
      [(list t) t]
      [(list n nts ...) (array-type n (apply array:impl nts))]))
  (λ ts
    (expect-values 'array ts)
    (define last-n (sub1 (length ts)))
    (for ([(t n) (in-indexed ts)])
      (if (= n last-n)
        (expect-type    'array n t)
        (expect-integer 'array n t)))
    (type:impl)))

(define-function record
  (λ args
    (record-type (apply hash args)))
  (λ ts
    (expect-values 'record ts)
    (for ([(t n) (in-indexed ts)])
      (if (even? n)
        (expect-symbol 'record n t)
        (expect-type   'record n t)))
    (type:impl)))

(define-function type
  (const (subtype-type (any-type)))
  (const (type:impl)))

(define-function subtype subtype-type
  (const (type:impl)))

(define-function bit
  (const (unsigned-type 1))
  (const (type:impl)))

(define-function natural
  (const (unsigned-type #f))
  (const (type:impl)))

(define-function integer
  (const (signed-type #f))
  (const (type:impl)))

(define-function enumeration
  (λ args
    (union-type (map symbol-type args)))
  (λ ts
    (expect-values  'enumeration ts)
    (expect-symbols 'enumeration ts)
    (type:impl)))
