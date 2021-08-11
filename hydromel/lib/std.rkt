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
  std/true?      true?-impl true?-impl-signature
  std/if         if-impl    if-impl-signature
  std/not        bitwise-not-signature
  std/and        bitwise-and-signature
  std/or         bitwise-ior-signature
  std/xor        bitwise-xor-signature
  std/==         ==-impl ==-impl-signature
  std//=         /=-impl /=-impl-signature
  std/>          >-impl >-impl-signature
  std/+          +-signature
  std/-          --signature
  std/*          *-signature
  std/range      range-impl range-impl-signature
  (all-from-out  "logic.rkt")
  signed_width   min-signed-width-signature
  unsigned_width min-unsigned-width-signature)

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax std/true? (meta/builtin-function #'true?-impl))

(define (true?-impl a)
  (not (zero? a)))

(define (true?-impl-signature t)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to std/if as if it were a function.
(define-syntax std/if (meta/builtin-function #'if-impl))

(define-syntax-parse-rule (if-impl (~seq c t) ... e)
  (cond [(true?-impl c) t]
        ...
        [else e]))

(define (if-impl-signature tc . ts)
  (t/union ts))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/builtin-function #'min-unsigned-width))

(define (min-unsigned-width-signature t)
  (t/unsigned 32)) ; TODO set a relevant width here

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/builtin-function #'min-signed-width))

(define (min-signed-width-signature t)
  (t/unsigned 32)) ; TODO set a relevant width here

; Boolean operators are all bitwise.
(define-syntax std/not (meta/builtin-function #'bitwise-not))
(define-syntax std/and (meta/builtin-function #'bitwise-and))
(define-syntax std/or  (meta/builtin-function #'bitwise-ior))
(define-syntax std/xor (meta/builtin-function #'bitwise-xor))

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
(define-syntax std/== (meta/builtin-function #'==-impl))
(define-syntax std//= (meta/builtin-function #'!=-impl))
(define-syntax std/>  (meta/builtin-function #'>-impl))

(define (==-impl a b)
  (if (= a b) 1 0))

(define (/=-impl a b)
  (if (= a b) 0 1))

(define (>-impl a b)
  (if (> a b) 1 0))

(define (comparison-signature ta tb)
  (t/unsigned 1))

(define ==-impl-signature comparison-signature)
(define /=-impl-signature comparison-signature)
(define >-impl-signature  comparison-signature)

; Use the built-in arithmetic operators.
(define-syntax std/+ (meta/builtin-function #'+))
(define-syntax std/- (meta/builtin-function #'-))
(define-syntax std/* (meta/builtin-function #'*))

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
(define-syntax std/range (meta/builtin-function #'range-impl))

(define (range-impl a b)
  (range a (add1 b)))

(define (range-impl-signature ta tb)
  (t/range
    (match (cons ta tb)
      [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
      [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (max (add1 na) nb))]
      [(cons (t/signed na)   (t/unsigned nb)) (t/signed   (max na (add1 nb)))]
      [(cons (t/signed   na) (t/signed   nb)) (t/signed   (max na nb))]
      [_ (error "Range expects integer boundaries.")])))
