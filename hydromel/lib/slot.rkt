; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(provide
  (struct-out slot)
  make-slot
  update-slot!
  slot-type
  slot-type*
  slot-data*)

; A slot contains a value and a function that computes its type.
; In most cases, use make-slot to construct a slot instance.
(struct slot (stx data declared-type typer) #:mutable #:transparent)

(define (make-slot stx data declared-type [actual-typer #f])
  (slot stx data declared-type (make-slot-typer declared-type actual-typer)))

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
