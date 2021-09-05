#lang racket

(require syntax/parse/define)

(provide
  (struct-out slot)
  make-slot
  slot-type
  connect)

; A slot contains a value and a function that computes its type.
; In most cases, use make-slot to construct a slot instance.
(struct slot (data typer) #:mutable #:transparent)

(define-syntax-parse-rule (make-slot data type)
  (let ([res #f]
        [visiting #f])
    (slot data (thunk
                 (when visiting
                   ; TODO display signal names, locate error in source code
                   (error "Could not infer type due to cross-dependencies"))
                 (unless res
                   (set! visiting #t)
                   (set! res type)
                   (set! visiting #f))
                 res))))

(define (slot-type slt)
  ((slot-typer slt)))

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
