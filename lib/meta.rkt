#lang racket

(require threading "scope.rkt")

(provide (all-defined-out))

(struct design-unit ([ports #:mutable]))

(define (design-unit-ref unit name [strict? #t])
  (define ports (design-unit-ports unit))
  ; Attempt to find a port with the given name in the current unit.
  (dict-ref ports (syntax-e name)
    (thunk
      ; If not found, look into each spliced composite port.
      ; Do not raise an error if not found.
      (define port (for/fold ([res #f])
                             ([p (in-list (dict-values ports))]
                              #:when (and (composite-port? p) (composite-port-splice? p))
                              #:break res)
                     (~> p
                         (composite-port-intf-name)
                         (lookup interface?)
                         (design-unit-ref name #f))))
      ; In strict mode, raise an error if no port was found at this point.
      (when (and strict? (not port))
        (raise-syntax-error #f "No port with this name" name))
      ; Return the port found.
      port)))

(struct interface design-unit ())
(struct component design-unit ())

(struct port (name))
(struct data-port      port (mode))
(struct composite-port port (intf-name flip? splice?))

(struct constant ())
(struct parameter ())

(struct local-signal ())

(struct instance (comp-name))

(define (signal? item)
  (or (data-port?    item)
      (local-signal? item)))
