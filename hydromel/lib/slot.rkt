#lang racket

(require syntax/parse/define)

(provide
  (struct-out slot)
  type-thunk
  slot-type)

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
