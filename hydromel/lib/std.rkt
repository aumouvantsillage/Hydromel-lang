#lang racket

(require
  "signal.rkt"
  "slot.rkt"
  "logic-vector.rkt"
  (prefix-in t/ "types.rkt")
  (prefix-in l/ "logic.rkt")
  syntax/parse/define)

(provide (all-defined-out))

(define (hydromel-true? a)
  (not (zero? a)))

(define (hydromel-true?-signature t)
  (t/boolean))

(define (hydromel-if-signature tc . ts)
  (t/union ts))

(define-syntax-parse-rule (hydromel-if (~seq c t) ... e)
  (cond [(hydromel-true? c) t]
        ...
        [else e]))

(define (signed-signature n)
  (t/type))

(define (unsigned-signature n)
  (t/type))

(define unsigned_width l/min-unsigned-width)

(define (unsigned_width-signature t)
  (t/unsigned 32)) ; TODO set a relevant width here

(define signed_width l/min-signed-width)

(define (signed_width-signature t)
  (t/unsigned 32)) ; TODO set a relevant width here

(define hydromel-==  logic-vector-==)
(define hydromel-/=  logic-vector-/=)
(define hydromel->   logic-vector->)
(define hydromel-not logic-vector-not)
(define hydromel-and logic-vector-and)
(define hydromel-or  logic-vector-or)
(define hydromel-xor logic-vector-xor)
(define hydromel--   logic-vector--)
(define hydromel-+   logic-vector-+)
(define hydromel-*   logic-vector-*)

(define hydromel-==-signature  logic-vector-==-signature)
(define hydromel-/=-signature  logic-vector-/=-signature)
(define hydromel->-signature   logic-vector->-signature)
(define hydromel-not-signature logic-vector-not-signature)
(define hydromel-and-signature logic-vector-and-signature)
(define hydromel-or-signature  logic-vector-or-signature)
(define hydromel-xor-signature logic-vector-xor-signature)
(define hydromel---signature   logic-vector---signature)
(define hydromel-+-signature   logic-vector-+-signature)
(define hydromel-*-signature   logic-vector-*-signature)

; TODO descending ranges
(define (hydromel-range a b)
  (range a (add1 b)))

(define (hydromel-range-signature ta tb)
  (t/range
    (match (cons ta tb)
      [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
      [(cons (t/signed   na) (t/integer  nb)) (t/signed   (max na nb))]
      [(cons (t/integer  na) (t/signed   nb)) (t/signed   (max na nb))]
      [_ 'invalid-and])))

(define (hydromel-connect left right)
  (for ([(k vl) (in-dict left)])
    (define vr (dict-ref right k))
    (cond
      [(slot? vl)
       (let ([sl (slot-signal vl)]
             [sr (slot-signal vr)])
         ; If one of the current items is a non-empty slot,
         ; copy the content of the non-empty slot into the empty one.
         ; If both items are empty slots, copy the left slot itself
         ; into the right dictionary.
         (cond [(and sl sr) (error "Cannot overwrite an existing connection at" k)]
               [sl          (set-slot-signal! vr sl)]
               [sr          (set-slot-signal! vl sr)]
               [else        (dict-set! right k vl)]))]

      ; If both items are dictionaries, connect their contents.
      [(dict? vl)
       (hydromel-connect vl vr)]

      [else (error "Unsupported connection at" k)])))
