; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  threading
  "scope.rkt"
  "errors.rkt")

(provide (all-defined-out))

(struct metadata (stx))

; Common parent struct for all design units.
; fields is a hash map whose keys are symbols and values are port and constant metadata.
(struct design-unit metadata (fields))

(define (design-unit-ref unit name [failure-thunk (const #f)])
  (define fields (design-unit-fields unit))
  ; Attempt to find a port with the given name in the current unit.
  (dict-ref fields (syntax-e name)
    (thunk
      ; If not found, look into each spliced composite port.
      (define port (for/fold ([res #f])
                             ([p (in-dict-values fields)]
                              #:when (and (composite-port? p) (composite-port-splice? p))
                              #:break res)
                     ; Find a port with the given name in the interface
                     ; of composite port p.
                     (define q (~> p
                                   composite-port-interface
                                   (design-unit-ref name)))
                     ; If a port was found and p is flipped, then flip q.
                     (if (and q (composite-port-flip? p))
                       (flip-port q)
                       q)))
      (or port (failure-thunk)))))

(struct interface design-unit ())
(struct component design-unit ())

(define (make-interface stx . fields)
  (interface stx (make-hash fields)))

(define (make-component stx . fields)
  (component stx (make-hash fields)))

(struct port           metadata ())
(struct data-port      port     (mode))
(struct composite-port port     (intf-name flip? splice?))

(define (lookup-interface stx name)
  (define res (lookup name))
  (if (interface? res)
    res
    (raise-semantic-error "Expected an interface name" stx name)))

(define (composite-port-interface port)
  (match-define (composite-port stx name _ _) port)
  (lookup-interface stx name))

; Returns a port with the same properties as p, but with flipped mode.
; If p is not a port, it is returned unchanged.
(define (flip-port p)
  (match p
    [(data-port      stx mode)
     (data-port      stx (if (eq? 'in mode) 'out 'in))]
    [(composite-port stx intf-name flip?       splice?)
     (composite-port stx intf-name (not flip?) splice?)]
    [_ p]))

(struct constant metadata (global?))

(struct parameter metadata ())

(struct local-signal metadata ())

(struct instance metadata (comp-name))

(define (lookup-component stx name)
  (define res (lookup name))
  (if (component? res)
    res
    (raise-semantic-error "Expected a component name" stx name)))

(define (instance-component inst)
  (match-define (instance stx name) inst)
  (lookup-component stx name))

(define (signal? item)
  (or (data-port?    item)
      (local-signal? item)))

(struct function metadata (name cast?))

(define (make-function stx name)
  (function stx name #f))

(define (make-function/cast stx name)
  (function stx name #t))
