; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require syntax/parse/define)

(provide
  (struct-out slot)
  slot-type
  slot-type*
  slot-data*
  connect)

; A slot contains a value and a function that computes its type.
; In most cases, use make-slot to construct a slot instance.
(struct slot (data declared-type actual-typer) #:mutable #:transparent)

(define (slot-type slt)
  ((slot-actual-typer slt) #f))

(define (slot-type* slt)
  ((slot-actual-typer slt) #t))

(define (slot-data* slt)
  (if (slot? slt)
    (slot-data slt)
    slt))

(define (connect left right)
  (for ([(k vl) (in-dict left)])
    (define vr (dict-ref right k))
    (cond
      [(slot? vl)
       (let ([sl (slot-data vl)]
             [sr (slot-data vr)])
         ; If one of the current items is a non-empty slot,
         ; copy the content of the non-empty slot into the empty one.
         ; If both items are empty slots, copy the left slot itself
         ; into the right dictionary.
         (cond [(and sl sr) (error "Cannot overwrite an existing connection at" k)]
               [sl          (set-slot-data! vr sl)]
               [sr          (set-slot-data! vl sr)]
               [else        (dict-set! right k vl)]))]

      ; If both items are dictionaries, connect their contents.
      [(dict? vl)
       (connect vl vr)]

      [else (error "Unsupported connection at" k)])))
