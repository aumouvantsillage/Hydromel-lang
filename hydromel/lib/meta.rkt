; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  threading
  "scope.rkt"
  "errors.rkt")

(provide
  (struct-out design-unit) design-unit-ref
  (struct-out interface) make-interface lookup-interface
  (struct-out component) make-component lookup-component
  (struct-out data-port)
  (struct-out composite-port) composite-port-interface
  flip-port
  (struct-out instance) instance-component
  (struct-out constant)
  (struct-out parameter)
  (struct-out local-signal) signal?
  (struct-out function) make-function make-function/cast)

; The base struct for all metadata.
; `stx` is the syntax object from which this metadata are derived.
; `stx` is used to provide context information when reporting errors.
(struct metadata (stx))

; The base struct for all design units.
; `fields` is a hash map whose keys are symbols and values are port and constant metadata.
(struct design-unit metadata (fields))

; Find a field with the given name in the given design unit and return its metadata.
; This function will also search inside spliced composite ports.
; If no field is found, `failure-thunk` is called and its result is returned.
(define (design-unit-ref unit name [failure-thunk (const #f)])
  (define fields (design-unit-fields unit))
  ; Attempt to find a field with the given name in the current unit.
  (dict-ref fields (syntax-e name)
    (thunk
      ; If not found, look into each spliced composite port.
      (define field (for/fold ([res #f])
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
      (or field (failure-thunk)))))

; An interface is a special kind of design unit.
(struct interface design-unit ())

; Helper constructor to create an interface from a list of fields
; provided as pairs `(name . metadata)`.
(define (make-interface stx . fields)
  (interface stx (make-hash fields)))

; Find an interface with the given name in the current scope.
; `stx` is the syntax object of the element that is undergoing semantic checking
; (e.g. a composite port) and is used for error reporting.
(define (lookup-interface stx name)
  (define res (lookup name))
  (if (interface? res)
    res
    (raise-semantic-error "Expected an interface name" stx name)))

; A component is a special kind of design unit.
(struct component design-unit ())

; Helper constructor to create a component from a list of fields
; provided as pairs `(name . metadata)`.
(define (make-component stx . fields)
  (component stx (make-hash fields)))

; Find a component with the given name in the current scope.
; `stx` is the syntax object of the element that is undergoing semantic checking
; (e.g. an instance) and is used for error reporting.
(define (lookup-component stx name)
  (define res (lookup name))
  (if (component? res)
    res
    (raise-semantic-error "Expected a component name" stx name)))

; The base struct for all ports.
(struct port metadata ())

; A data port has a mode ('in, 'out).
(struct data-port port (mode))

; A composite port refers to an interface.
; It can be flipped or spliced.
(struct composite-port port (intf-name flip? splice?))

; Returns a port with the same properties as `p`, but with flipped mode.
; If `p` is not a port, it is returned unchanged.
(define (flip-port p)
  (match p
    [(data-port      stx mode)
     (data-port      stx (if (eq? 'in mode) 'out 'in))]
    [(composite-port stx intf-name flip?       splice?)
     (composite-port stx intf-name (not flip?) splice?)]
    [_ p]))

; Find the metadata of the interface referenced in a composite port.
(define (composite-port-interface port)
  (match-define (composite-port stx name _ _) port)
  (lookup-interface stx name))

; An instance refers to a component.
(struct instance metadata (comp-name))

; Find the metadata of the component referenced in an instance.
(define (instance-component inst)
  (match-define (instance stx name) inst)
  (lookup-component stx name))

; A constant can be local to a design unit, or global.
(struct constant metadata (global?))

; This struct represents parameters in design units, functions and type definitions.
(struct parameter metadata ())

; This struct represents local signals in components.
(struct local-signal metadata ())

; Does the given item transport a signal?
; Signals are associated with data port and local signals.
(define (signal? item)
  (or (data-port?    item)
      (local-signal? item)))

; The metadata of a Hydromel function contains the name of a Racket function
; that implements it. If `cast?` is true, the result of the function must be
; explicitly cast to the return type specified by the function.
(struct function metadata (name cast?))

; Helper constructor for functions that do not need an explicit cast.
(define (make-function stx name)
  (function stx name #f))

; Helper constructor for functions that need an explicit cast.
(define (make-function/cast stx name)
  (function stx name #t))
