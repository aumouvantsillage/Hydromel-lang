#lang racket

(require
  "signal.rkt"
  "logic-vector.rkt"
  syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (hydromel-if (~seq c t) ... e)
  (cond [(logic-vector-true? c) t]
        ...
        [else e]))

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

; TODO descending ranges
(define (hydromel-range a b)
  (map make-logic-vector (range (logic-vector-value a) (add1 (logic-vector-value b)))))

(define (hydromel-connect left right)
  (for ([(k vl) (in-dict left)])
    (define vr (dict-ref right k))
    (cond
      [(box? vl)
       (let ([sl (unbox vl)]
             [sr (unbox vr)])
         ; If one of the current items is a non-empty box,
         ; copy the content of the non-empty box into the empty one.
         ; If both items are empty boxes, copy the left box itself
         ; into the right dictionary.
         (cond [(and sl sr) (error "Cannot overwrite an existing connection at" k)]
               [sl          (set-box! vr sl)]
               [sr          (set-box! vl sr)]
               [else        (dict-set! right k vl)]))]

      ; If both items are dictionaries, connect their contents.
      [(dict? vl)
       (hydromel-connect vl vr)]

      [else (error "Unsupported connection at" k)])))
