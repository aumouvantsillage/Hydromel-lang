#lang racket

(require
  "signal.rkt"
  "logic-vector.rkt"
  syntax/parse/define)

(provide (all-defined-out))

; A slot contains a signal and its type.
; The type-thunk field is a function that returns the type of the current signal.
(struct slot (signal type-fn) #:mutable)

(define-syntax-parse-rule (type-thunk expr)
  (let ([res #f]
        [visiting #f])
    (thunk
      (when visiting
        ; TODO display signal names, locate error in source code
        (error "Could not infer type due to cross-dependencies"))
      (unless res
        (set! visiting #t)
        (set! res expr)
        (set! visiting #f))
      res)))

(define (slot-type slt)
  ((slot-type-fn slt)))

(define-syntax-parse-rule (hydromel-if (~seq c t) ... e)
  (cond [(logic-vector-true? c) t]
        ...
        [else e]))

(define unsigned_width logic-vector-unsigned-width)

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
  (map make-logic-vector (range (logic-vector-value a) (add1 (logic-vector-value b)))))

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
