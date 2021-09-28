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
  int-to-bool    int-to-bool-impl int-to-bool-impl-return-type
  kw-if          kw-if-impl       kw-if-impl-return-type
  kw-not                          bitwise-not-return-type
  kw-and                          bitwise-and-return-type
  kw-or                           bitwise-ior-return-type
  kw-xor                          bitwise-xor-return-type
  kw-==          kw-==-impl       kw-==-impl-return-type
  kw-/=          kw-/=-impl       kw-/=-impl-return-type
  kw->           kw->-impl        kw->-impl-return-type
  kw-+                            +-return-type
  kw--                            --return-type
  kw-*                            *-return-type
  kw-/                            quotient-return-type
  kw-range       kw-range-impl    kw-range-impl-return-type
  kw-slice                        unsigned-slice-return-type
  kw-concat      kw-concat-impl   kw-concat-impl-return-type
  kw-array                        vector-return-type
  kw-array-ref                    vector-ref-return-type
  (all-from-out  "logic.rkt")
  signed_width                    min-signed-width-return-type
  unsigned_width                  min-unsigned-width-return-type
  cast           cast-impl        cast-impl-return-type)

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax int-to-bool (meta/builtin-function #'int-to-bool-impl))

(define (int-to-bool-impl a)
  (not (zero? a)))

(define (int-to-bool-impl-return-type ta)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to kw-if as if it were a function.
(define-syntax kw-if (meta/builtin-function #'kw-if-impl))

(define-syntax-parse-rule (kw-if-impl (~seq cnd thn) ... els)
  (cond [(int-to-bool-impl cnd) thn]
        ...
        [else els]))

(define (kw-if-impl-return-type tc . ts)
  (t/union ts))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/builtin-function #'min-unsigned-width))

(define (min-unsigned-width-return-type ta)
  (define ta^ (t/normalize-type ta))
  (t/unsigned (t/abstract-integer-width ta^)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/builtin-function #'min-signed-width))

(define (min-signed-width-return-type ta)
  (define ta^ (t/normalize-type ta))
  (t/unsigned (match ta^
                [(t/signed   n) n]
                [(t/unsigned n) (add1 n)]
                [_ (error "Cannot compute data size.")])))

; Boolean operators are all bitwise.
(define-syntax kw-not (meta/builtin-function #'bitwise-not))
(define-syntax kw-and (meta/builtin-function #'bitwise-and))
(define-syntax kw-or  (meta/builtin-function #'bitwise-ior))
(define-syntax kw-xor (meta/builtin-function #'bitwise-xor))

(define (bitwise-return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na)          (t/unsigned nb))          (t/unsigned (max na nb))]
    [(list (t/signed   na)          (t/abstract-integer  nb)) (t/signed   (max na nb))]
    [(list (t/abstract-integer  na) (t/signed   nb))          (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define (bitwise-not-return-type ta) ta)
(define bitwise-and-return-type bitwise-return-type)
(define bitwise-ior-return-type bitwise-return-type)
(define bitwise-xor-return-type bitwise-return-type)

; Comparison operations return integers 0 and 1.
(define-syntax kw-== (meta/builtin-function #'kw-==-impl))
(define-syntax kw-/= (meta/builtin-function #'kw-/=-impl))
(define-syntax kw->  (meta/builtin-function #'kw->-impl))

(define (kw-==-impl a b)
  (if (= a b) 1 0))

(define (kw-/=-impl a b)
  (if (= a b) 0 1))

(define (kw->-impl a b)
  (if (> a b) 1 0))

(define (comparison-return-type ta tb)
  (t/unsigned 1))

(define kw-==-impl-return-type comparison-return-type)
(define kw-/=-impl-return-type comparison-return-type)
(define kw->-impl-return-type  comparison-return-type)

; Use the built-in arithmetic operators.
(define-syntax kw-+ (meta/builtin-function #'+))
(define-syntax kw-- (meta/builtin-function #'-))
(define-syntax kw-* (meta/builtin-function #'*))
(define-syntax kw-/ (meta/builtin-function #'quotient))

(define (+-return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (max na nb)))]
    [(list (t/unsigned na) (t/signed   nb)) (t/signed   (add1 (max (add1 na) nb)))]
    [(list (t/signed   na) (t/unsigned nb)) (t/signed   (add1 (max na (add1 nb))))]
    [(list (t/signed   na) (t/signed   nb)) (t/signed   (add1 (max na nb)))]
    [_ (error "Arithmetic operation expects integer operands." ta^ tb^)]))

(define (--return-type ta [tb #f])
  (if tb
    (+-return-type ta tb)
    (t/signed (add1 (t/abstract-integer-width (t/normalize-type ta))))))

(define (*-return-type ta tb)
  (define ta^ (t/normalize-type ta))
  (define tb^ (t/normalize-type tb))
  (match (list ta^ tb^)
    [(list (t/unsigned na)         (t/unsigned nb))          (t/unsigned (+ na nb))]
    [(list (t/abstract-integer na) (t/signed   nb))          (t/signed   (+ na nb))]
    [(list (t/signed na)           (t/abstract-integer  nb)) (t/signed   (+ na nb))]
    [_ (error "Arithmetic operation expects integer operands.")]))

(define (quotient-return-type ta tb)
  ta)

; TODO descending ranges
(define-syntax kw-range (meta/builtin-function #'kw-range-impl))

(define (kw-range-impl a b)
  (range a (add1 b)))

(define (kw-range-impl-return-type ta tb)
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
(define-syntax kw-slice (meta/builtin-function #'unsigned-slice))

(define (unsigned-slice-return-type ta tb tc)
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

(define-syntax kw-concat (meta/builtin-function #'kw-concat-impl))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the return-type.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define-syntax-parse-rule (kw-concat-impl (~seq a ta) ...)
  (unsigned-concat [a (~> ta t/normalize-type t/abstract-integer-width sub1) 0] ...))

(define (kw-concat-impl-return-type . ts)
  (define ts^ (for/list ([it (in-list ts)]
                         [n (in-naturals)] #:when (odd? n))
                (~> it t/static-data-value t/normalize-type)))
  (define w (for/sum ([it (in-list ts^)])
              ; TODO assert that it contains an integer type
              (t/abstract-integer-width it)))
  (match (first ts^)
    [(t/signed _)   (t/signed w)]
    [(t/unsigned _) (t/unsigned w)]))

(define-syntax kw-array (meta/builtin-function #'vector))

(define (vector-return-type . ts)
  (t/array (length ts) (t/union ts)))

(define-syntax kw-array-ref (meta/builtin-function #'vector-ref))

(define (vector-ref-return-type ta tb)
  ; TODO check the type of tb
  (define ta^ (t/normalize-type ta))
  (match ta^
    [(t/array _ te) te]
    [_              (error "Not an array type" ta)]))

(define-syntax cast (meta/builtin-function #'cast-impl))

; cast does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(define (cast-impl a b)
  b)

(define (cast-impl-return-type ta tb)
  (t/static-data-value ta))
