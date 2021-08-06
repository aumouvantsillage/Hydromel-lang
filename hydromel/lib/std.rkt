#lang racket

(require
  "signal.rkt"
  "slot.rkt"
  (prefix-in t/ "types.rkt")
  (prefix-in l/ "logic.rkt")
  syntax/parse/define)

(provide
  hydromel-true? hydromel-true?-signature
  hydromel-if    hydromel-if-signature
  signed-signature
  unsigned-signature
  signed_width   signed_width-signature
  unsigned_width unsigned_width-signature
  hydromel-not   hydromel-not-signature
  hydromel-and   hydromel-and-signature
  hydromel-or    hydromel-or-signature
  hydromel-xor   hydromel-xor-signature
  hydromel-==    hydromel-==-signature
  hydromel-/=    hydromel-/=-signature
  hydromel->     hydromel->-signature
  hydromel-+     hydromel-+-signature
  hydromel--     hydromel---signature
  hydromel-*     hydromel-*-signature
  hydromel-range hydromel-range-signature)

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define (hydromel-true? a)
  (not (zero? a)))

(define (hydromel-true?-signature t)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to hydromel-if as if it were a function.
(define-syntax-parse-rule (hydromel-if (~seq c t) ... e)
  (cond [(hydromel-true? c) t]
        ...
        [else e]))

(define (hydromel-if-signature tc . ts)
  (t/union ts))

; Parameterized data types are exposed as functions
; whose result is a type.
(define (signed-signature n)
  (t/type))

(define (unsigned-signature n)
  (t/type))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define unsigned_width l/min-unsigned-width)

(define (unsigned_width-signature t)
  (t/unsigned 32)) ; TODO set a relevant width here

; Returns the minimum width to encode a given number
; as an signed integer.
(define signed_width l/min-signed-width)

(define (signed_width-signature t)
  (t/unsigned 32)) ; TODO set a relevant width here

; Boolean operators are all bitwise.
(define hydromel-not bitwise-not)
(define hydromel-and bitwise-and)
(define hydromel-or  bitwise-ior)
(define hydromel-xor bitwise-xor)

(define (bitwise-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define (hydromel-not-signature ta) ta)
(define hydromel-and-signature bitwise-signature)
(define hydromel-or-signature  bitwise-signature)
(define hydromel-xor-signature bitwise-signature)

; Comparison operations return integers 0 and 1.
(define (hydromel-== a b)
  (if (= a b) 1 0))

(define (hydromel-/= a b)
  (if (= a b) 0 1))

(define (hydromel-> a b)
  (if (> a b) 1 0))

(define (comparison-signature ta tb)
  (t/unsigned 1))

(define hydromel-==-signature comparison-signature)
(define hydromel-/=-signature comparison-signature)
(define hydromel->-signature  comparison-signature)

; Use the built-in arithmetic operators.
(define hydromel-+ +)
(define hydromel-- -)
(define hydromel-* *)

(define (hydromel-+-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (max na nb)))]
    [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (add1 (max (add1 na) nb)))]
    [(cons (t/signed   na) (t/unsigned nb)) (t/signed   (add1 (max na (add1 nb))))]
    [(cons (t/signed  na)  (t/signed   nb)) (t/signed   (add1 (max na nb)))]
    [_ (error "Arithmetic operation expects integer operands.")]))

(define (hydromel---signature ta [tb #f])
  (if tb
    (hydromel-+-signature ta tb)
    (t/signed (add1 (t/integer-width ta)))))

(define (hydromel-*-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (+ na nb))]
    [(cons (t/integer na)  (t/signed   nb)) (t/signed   (+ na nb))]
    [(cons (t/signed na)   (t/integer  nb)) (t/signed   (+ na nb))]
    [_ (error "Arithmetic operation expects integer operands.")]))

; TODO descending ranges
(define (hydromel-range a b)
  (range a (add1 b)))

(define (hydromel-range-signature ta tb)
  (t/range
    (match (cons ta tb)
      [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
      [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (max (add1 na) nb))]
      [(cons (t/signed na)   (t/unsigned nb)) (t/signed   (max na (add1 nb)))]
      [(cons (t/signed   na) (t/signed   nb)) (t/signed   (max na nb))]
      [_ (error "Range expects integer boundaries.")])))
