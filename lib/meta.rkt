#lang racket

(require syntax/id-table "scope.rkt")

(provide (all-defined-out))

(struct design-unit (public-fields))

(define (design-unit-set-from-scope! unit name)
  (dict-set! (design-unit-public-fields unit) (syntax-e name) (lookup name)))

(define (design-unit-ref unit name)
  (dict-ref (design-unit-public-fields unit) (syntax-e name)
    (thunk (raise-syntax-error #f "No element with this name" name))))

(struct interface design-unit ())

(define (make-interface)
  (interface (make-hash)))

(struct component design-unit ())

(define (make-component)
  (component (make-hash)))

(struct field (name))

(struct constant ())
(struct parameter field ())

(struct data-port field (mode))
(struct local-signal ())

(struct composite-port field (intf-name flip?))
(struct instance (comp-name))

(define (signal? item)
  (or (data-port? item)
      (local-signal? item)))
