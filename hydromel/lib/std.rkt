; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "signal.rkt"
  "slot.rkt"
  (prefix-in t/ "types.rkt")
  (only-in "logic.rkt"
    min-unsigned-width min-signed-width
    min-signed-value   max-signed-value
    min-unsigned-value max-unsigned-value
    unsigned-slice     unsigned-concat)
  syntax/parse/define
  threading
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide
  int-to-bool    int-to-bool:impl int-to-bool:impl:return-type
  &if            if:impl          if:impl:return-type
  &not                            bitwise-not:return-type
  &and                            bitwise-and:return-type
  &or                             bitwise-ior:return-type
  &xor                            bitwise-xor:return-type
  &==            eq:impl          eq:impl:return-type
  &/=            ne:impl          ne:impl:return-type
  &>             gt:impl          gt:impl:return-type
  &+                              +:return-type
  &-                              -:return-type
  &*                              *:return-type
  &/                              quotient:return-type
  &range         range:impl       range:impl:return-type
  slice                           unsigned-slice:return-type
  concat         concat:impl      concat:impl:return-type
  make-array                      vector:return-type
  array-ref                       vector-ref:return-type
  (all-from-out  "logic.rkt")
  signed_width                    min-signed-width:return-type
  unsigned_width                  min-unsigned-width:return-type
  cast           cast:impl        cast:impl:return-type)

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax int-to-bool (meta/builtin-function #'int-to-bool:impl))

(define (int-to-bool:impl a)
  (not (zero? a)))

(define (int-to-bool:impl:return-type ta)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to &if as if it were a function.
(define-syntax &if (meta/builtin-function #'if:impl))

(define-syntax-parse-rule (if:impl (~seq cnd thn) ... els)
  (cond [(int-to-bool:impl cnd) thn]
        ...
        [else els]))

(define (if:impl:return-type tc . ts)
  (t/union ts))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/builtin-function #'min-unsigned-width))

(define (min-unsigned-width:return-type ta)
  (define ta^ (t/normalize-type ta))
  (t/unsigned (t/abstract-integer-width ta^)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/builtin-function #'min-signed-width))

(define (min-signed-width:return-type ta)
  (define ta^ (t/normalize-type ta))
  (t/unsigned (match ta^
                [(t/signed   n) n]
                [(t/unsigned n) (add1 n)]
                [_ (error "Cannot compute data size.")])))

; Boolean operators are all bitwise.
(define-syntax &not (meta/builtin-function #'bitwise-not))
(define-syntax &and (meta/builtin-function #'bitwise-and))
(define-syntax &or  (meta/builtin-function #'bitwise-ior))
(define-syntax &xor (meta/builtin-function #'bitwise-xor))

(define (bitwise:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na)          (t/unsigned nb))          (t/unsigned (max na nb))]
    [(list (t/signed   na)          (t/abstract-integer  nb)) (t/signed   (max na nb))]
    [(list (t/abstract-integer  na) (t/signed   nb))          (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define (bitwise-not:return-type ta) ta)
(define bitwise-and:return-type bitwise:return-type)
(define bitwise-ior:return-type bitwise:return-type)
(define bitwise-xor:return-type bitwise:return-type)

; Comparison operations return integers 0 and 1.
(define-syntax &== (meta/builtin-function #'eq:impl))
(define-syntax &/= (meta/builtin-function #'ne:impl))
(define-syntax &>  (meta/builtin-function #'gt:impl))

(define (eq:impl a b)
  (if (= a b) 1 0))

(define (ne:impl a b)
  (if (= a b) 0 1))

(define (gt:impl a b)
  (if (> a b) 1 0))

(define (comparison:return-type ta tb)
  (t/unsigned 1))

(define eq:impl:return-type comparison:return-type)
(define ne:impl:return-type comparison:return-type)
(define gt:impl:return-type comparison:return-type)

; Use the built-in arithmetic operators.
(define-syntax &+ (meta/builtin-function #'+))
(define-syntax &- (meta/builtin-function #'-))
(define-syntax &* (meta/builtin-function #'*))
(define-syntax &/ (meta/builtin-function #'quotient))

(define (+:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (max na nb)))]
    [(list (t/unsigned na) (t/signed   nb)) (t/signed   (add1 (max (add1 na) nb)))]
    [(list (t/signed   na) (t/unsigned nb)) (t/signed   (add1 (max na (add1 nb))))]
    [(list (t/signed   na) (t/signed   nb)) (t/signed   (add1 (max na nb)))]
    [_ (error "Arithmetic operation expects integer operands." ta^ tb^)]))

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

; TODO descending ranges
(define-syntax &range (meta/builtin-function #'range:impl))

(define (range:impl a b)
  (range a (add1 b)))

(define (range:impl:return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (t/range
    (match (list ta^ tb^)
      [(list (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
      [(list (t/unsigned na) (t/signed   nb)) (t/signed   (max (add1 na) nb))]
      [(list (t/signed   na) (t/unsigned nb)) (t/signed   (max na (add1 nb)))]
      [(list (t/signed   na) (t/signed   nb)) (t/signed   (max na nb))]
      [_ (error "Range expects integer boundaries.")])))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
(define-syntax slice (meta/builtin-function #'unsigned-slice))

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
  (define width (max 0 (add1 (- left right))))
  (match (t/normalize-type ta)
    [(t/unsigned _) (t/unsigned width)]
    [(t/signed   _) (t/signed   width)]
    [_ (error "Slice expects integer value.")]))

(define-syntax concat (meta/builtin-function #'concat:impl))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
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

(define-syntax make-array (meta/builtin-function #'vector))

(define (vector:return-type . ts)
  (t/array (length ts) (t/union ts)))

(define-syntax array-ref (meta/builtin-function #'vector-ref))

(define (vector-ref:return-type ta tb)
  ; TODO check the type of tb
  (define ta^ (t/normalize-type ta))
  (match ta^
    [(t/array _ te) te]
    [_              (error "Not an array type" ta)]))

(define-syntax cast (meta/builtin-function #'cast:impl))

; cast does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(define (cast:impl a b)
  b)

(define (cast:impl:return-type ta tb)
  (t/static-data-value ta))
