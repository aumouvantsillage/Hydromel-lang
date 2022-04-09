; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require syntax/parse/define)

(provide
  (struct-out slot)
  make-slot
  update-slot!
  slot-type
  slot-type*
  slot-data*
  connect)

; A slot contains a value and a function that computes its type.
; In most cases, use make-slot to construct a slot instance.
(struct slot (data declared-type typer) #:mutable #:transparent)

(define (make-slot data declared-type [actual-typer #f])
  (slot data declared-type (make-slot-typer declared-type actual-typer)))

(define (make-slot-typer default-type actual-typer)
  (if actual-typer
    (let ([res      #f]
          [visiting #f])
      (λ (actual)
        (cond [(and default-type (not actual)) default-type]
              [res res]
              ; TODO display signal names, locate error in source code
              [visiting (or default-type (error "Could not infer type due to cross-dependencies"))]
              [else (set! visiting #t)
                    (set! res (actual-typer))
                    (set! visiting #f)
                    res])))
    (const default-type)))

(define (update-slot! slt data actual-typer)
  (set-slot-data! slt data)
  (set-slot-typer! slt (make-slot-typer (slot-declared-type slt) actual-typer)))

(define (slot-type slt)
  ((slot-typer slt) #f))

(define (slot-type* slt)
  ((slot-typer slt) #t))

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
         ; TODO Add type checking.
         (cond [(and sl sr) (error "Cannot overwrite an existing connection at" k)]
               [sl          (set-slot-data! vr sl)]
               [sr          (set-slot-data! vl sr)]
               [else        (dict-set! right k vl)]))]

      ; If both items are dictionaries, connect their contents.
      [(dict? vl)
       (connect vl vr)]

      [else (error "Unsupported connection at" k)])))
