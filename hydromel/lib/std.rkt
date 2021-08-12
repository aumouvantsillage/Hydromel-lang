#lang racket

(require
  "signal.rkt"
  "slot.rkt"
  (prefix-in t/ "types.rkt")
  (only-in "logic.rkt" min-unsigned-width min-signed-width)
  syntax/parse/define
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide
  int-to-bool    int-to-bool-impl int-to-bool-impl-signature
  kw-if          kw-if-impl       kw-if-impl-signature
  kw-not                          bitwise-not-signature
  kw-and                          bitwise-and-signature
  kw-or                           bitwise-ior-signature
  kw-xor                          bitwise-xor-signature
  kw-==          kw-==-impl       kw-==-impl-signature
  kw-/=          kw-/=-impl       kw-/=-impl-signature
  kw->           kw->-impl        kw->-impl-signature
  kw-+                            +-signature
  kw--                            --signature
  kw-*                            *-signature
  kw-range       kw-range-impl    kw-range-impl-signature
  (all-from-out  "logic.rkt")
  signed_width                    min-signed-width-signature
  unsigned_width                  min-unsigned-width-signature
  cast           cast-impl        cast-impl-signature)

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax int-to-bool (meta/builtin-function #'int-to-bool-impl))

(define (int-to-bool-impl a)
  (not (zero? a)))

(define (int-to-bool-impl-signature t)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to kw-if as if it were a function.
(define-syntax kw-if (meta/builtin-function #'kw-if-impl))

(define-syntax-parse-rule (kw-if-impl (~seq c t) ... e)
  (cond [(int-to-bool-impl c) t]
        ...
        [else e]))

(define (kw-if-impl-signature tc . ts)
  (t/union ts))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/builtin-function #'min-unsigned-width))

(define (min-unsigned-width-signature t)
  (t/unsigned (t/integer-width t)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/builtin-function #'min-signed-width))

(define (min-signed-width-signature t)
  (t/unsigned (match t
                [(t/signed   n) n]
                [(t/unsigned n) (add1 n)]
                [_ (error "Cannot compute data size.")])))

; Boolean operators are all bitwise.
(define-syntax kw-not (meta/builtin-function #'bitwise-not))
(define-syntax kw-and (meta/builtin-function #'bitwise-and))
(define-syntax kw-or  (meta/builtin-function #'bitwise-ior))
(define-syntax kw-xor (meta/builtin-function #'bitwise-xor))

(define (bitwise-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define (bitwise-not-signature ta) ta)
(define bitwise-and-signature bitwise-signature)
(define bitwise-ior-signature bitwise-signature)
(define bitwise-xor-signature bitwise-signature)

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

(define (comparison-signature ta tb)
  (t/unsigned 1))

(define kw-==-impl-signature comparison-signature)
(define kw-/=-impl-signature comparison-signature)
(define kw->-impl-signature  comparison-signature)

; Use the built-in arithmetic operators.
(define-syntax kw-+ (meta/builtin-function #'+))
(define-syntax kw-- (meta/builtin-function #'-))
(define-syntax kw-* (meta/builtin-function #'*))

(define (+-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (max na nb)))]
    [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (add1 (max (add1 na) nb)))]
    [(cons (t/signed   na) (t/unsigned nb)) (t/signed   (add1 (max na (add1 nb))))]
    [(cons (t/signed  na)  (t/signed   nb)) (t/signed   (add1 (max na nb)))]
    [_ (error "Arithmetic operation expects integer operands.")]))

(define (--signature ta [tb #f])
  (if tb
    (+-signature ta tb)
    (t/signed (add1 (t/integer-width ta)))))

(define (*-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (+ na nb))]
    [(cons (t/integer na)  (t/signed   nb)) (t/signed   (+ na nb))]
    [(cons (t/signed na)   (t/integer  nb)) (t/signed   (+ na nb))]
    [_ (error "Arithmetic operation expects integer operands.")]))

; TODO descending ranges
(define-syntax kw-range (meta/builtin-function #'kw-range-impl))

(define (kw-range-impl a b)
  (range a (add1 b)))

(define (kw-range-impl-signature ta tb)
  (t/range
    (match (cons ta tb)
      [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
      [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (max (add1 na) nb))]
      [(cons (t/signed na)   (t/unsigned nb)) (t/signed   (max na (add1 nb)))]
      [(cons (t/signed   na) (t/signed   nb)) (t/signed   (max na nb))]
      [_ (error "Range expects integer boundaries.")])))

(define-syntax cast (meta/builtin-function #'cast-impl))

(define (cast-impl t e)
  (t e))

(define (cast-impl-signature ta tb)
  (t/type-supertype ta))
