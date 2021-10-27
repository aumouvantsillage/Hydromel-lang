; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/stxparam
  (for-syntax
    syntax/parse/lib/function-header))

(provide
  signal?
  signal-defer
  signal-cons
  signal-first
  signal-rest
  signal-take
  list->signal
  signal
  signal-lift
  signal-lift*
  signal-λ
  define-signal
  for/signal
  register
  register/r
  register/e
  register/re
  this-reg
  signal-bundle
  signal-unbundle
  signal-bundle-list
  signal-bundle-vector
  >>)

(struct signal (body)
  #:name             private-signal
  #:constructor-name private-signal
  #:property         prop:procedure (struct-field-index body))

; Wrap the given body in a promise.
; body ... is supposed to evaluate to a truthy value.
(define-syntax-parse-rule (signal-delay body ...)
  (private-signal
    (let ([res #f])                    ; Will store the value of the promise.
      (thunk
        (unless res                    ; If the promise has not been forced,
          (set! res (begin body ...))) ; compute its value and store it.
        res))))                        ; Return the stored value.

; Evaluate a signal.
(define-syntax-parse-rule (signal-force sig)
  (sig))                               ; Call the λ created by signal-delay.

; Delay the evaluation of a signal.
; This can be used when we need to reference a signal that is constructed later.
(define-syntax-parse-rule (signal-defer sig)
  (private-signal (thunk (signal-force sig))))

; Construct a signal with a given value, followed by the given signal.
(define-syntax-parse-rule (signal-cons val sig)
  (signal-delay (cons val sig)))

; Evaluate the first sample of a signal.
(define (signal-first sig)
  (car (signal-force sig))) ; Returns the left element of the pair.

; Get a signal that starts at the second sample of the given signal.
(define (signal-rest sig)
  (cdr (signal-force sig))) ; Returns the right element of the pair.

; Returns a list of the first n samples of a signal.
(define (signal-take sig n)
  (if (positive? n)
    (cons                                       ; Make a list with:
      (signal-first sig)                        ; the value of the first sample,
      (signal-take (signal-rest sig) (sub1 n))) ; the next n-1 sample values.
    empty))

; Convert a list to a signal.
; The last element of lst is repeated indefinitely.
(define (list->signal lst)
  (define rst (rest lst))
  (define sig (signal-cons              ; Create a signal:
                (first lst)             ; with the first element of the list,
                (if (empty? rst)        ; and if there are no more samples,
                  sig                   ; cycle over the current signal,
                  (list->signal rst)))) ; else, create a signal with the rest of the list.
  sig)

; Create a signal with the given values.
; The last value is repeated indefinitely.
(define-syntax-parse-rule (signal val ...)
  (list->signal (list val ...))) ; Pass the arguments as a list to list->signal.


; ------------------------------------------------------------------------------
; Lifting.
; ------------------------------------------------------------------------------

; Convert f into a function that operates on signals.
; f must be a function.
; The resulting function takes any number of arguments.
(define (signal-lift f)
  (define (f^ . sig-lst)                      ; The lifted version of f takes any number of arguments.
    (signal-cons                              ; It will return a signal:
      (apply f  (map signal-first sig-lst))   ; with f applied to the first sample of each argument,
      (apply f^ (map signal-rest  sig-lst)))) ; and the lifted f applied to the rest of each argument.
  f^)

; Convert f into a function that operates on signals.
; f is not required to be a function.
; The resulting function takes the specified number of arguments.
(define-syntax-parse-rule (signal-lift* f arg ...)
  #:with (tmp ...) (generate-temporaries #'(arg ...)) ; Make a unique name for each argument.
  (letrec ([f^ (λ (tmp ...)                           ; The lifted version of f takes the given number of arguments.
                 (signal-cons                         ; It will return a signal:
                   (f  (signal-first tmp) ...)        ; with f applied to the first sample of each argument,
                   (f^ (signal-rest  tmp) ...)))])    ; and the lifted f applied to the rest of each argument.
    f^))

(begin-for-syntax
  (define-splicing-syntax-class signal-returns-clause
    (pattern (~seq #:returns (ret ...+))
      #:attr ret-len #`#,(length (attribute ret)))))

; Create an anonymous function that operates on signals.
(define-syntax-parser signal-λ
  [(signal-λ args :signal-returns-clause body ...)
   #'(compose1
       (curryr signal-unbundle ret-len)
       (signal-λ args
         body ...
         (list ret ...)))]
  ; Lift a λ with the given list of arguments.
  [(signal-λ (sig:id ...) body ...)
   #'(signal-lift* (λ (sig ...) body ...) sig ...)]
  ; Lift a λ that accepts any number of arguments.
  [(signal-λ sig-lst:id body ...)
   #'(signal-lift (λ sig-lst body ...))]
  ; Lift a λ that accepts keyword arguments.
  ; This form will not work with a rest argument.
  [(signal-λ f:formals body ...)
   #'(λ f (for/signal f.params body ...))])

; Define a signal or a function that operates on signals.
(define-syntax-parser define-signal
  ; Define a variable that contains a signal.
  [(define-signal name:id val ...)
   #'(define name (signal val ...))]
  ; Define a function with the given list of arguments.
  [(define-signal (name:id sig:id ...) body ...)
   #'(define name (signal-λ (sig ...) body ...))]
  ; Define a function that accepts any number of arguments.
  [(define-signal (name:id . sig-lst:id) body ...)
   #'(define name (signal-λ sig-lst body ...))]
  ; Define a function that accepts keyword arguments.
  ; This form will not work with a rest argument.
  [(define-signal h:function-header body ...)
   #'(define h (for/signal h.params body ...))])

; Apply the given body to the elements of one or more signals.
(begin-for-syntax
  (define-syntax-class signal-for-clause
    (pattern [var:id sig])
    (pattern var:id #:attr sig #'var)))

(define-syntax-parse-rule (for/signal (c:signal-for-clause ...) body ...)
  ; Create a lifted λ and apply it immediately to the given signals.
  ((signal-λ (c.var ...) body ...) (signal-defer c.sig) ...))

(define-syntax-parse-rule (>> name:id arg ...)
  ((signal-lift* name arg ...) (signal-defer arg) ...))


; ------------------------------------------------------------------------------
; Registers.
; ------------------------------------------------------------------------------

(define-syntax-parameter this-reg
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside register")))

; Register the given signal.
; The initial value of the result is q0.
; The expression can refer to the result as `this-reg`.
(define-syntax-parse-rule (register q0 expr)
  (letrec ([res (signal-cons q0
                  (syntax-parameterize ([this-reg (make-rename-transformer #'res)])
                    expr))])
    res))

; Register with synchronous reset.
; The resulting signal receives q0 each time sig-r is true.
(define-syntax-parse-rule (register/r q0 sig-r sig-d)
  (register q0 (for/signal ([r sig-r] [d sig-d])
                 (if r q0 d))))

; Register with enable.
; The resulting signal receives sig-d each time sig-e is true.
(define-syntax-parse-rule (register/e q0 sig-e sig-d)
  (register q0 (for/signal ([e sig-e] [d sig-d] [q this-reg])
                 (if e d q))))

; Register with synchronous reset and enable.
(define-syntax-parse-rule (register/re q0 sig-r sig-e sig-d)
  (register q0 (for/signal ([r sig-r] [e sig-e] [d sig-d] [q this-reg])
                 (cond [r    q0]
                       [e    d]
                       [else q]))))


; ------------------------------------------------------------------------------
; Bundling/unbundling.
; ------------------------------------------------------------------------------

; Convert one or more signals into a signal where each element is a list.
(define signal-bundle (signal-lift list))

(define (signal-bundle-list lst)
  (apply signal-bundle lst))

; For signals with list values. Return signals with the first or the rest
; of each sample.
(define signal-first^ (signal-lift* first _))
(define signal-rest^  (signal-lift* rest  _))

; Convert a signal of lists into a list of signals.
; Return the resulting signals as values.
(define (signal-unbundle sig len)
  (apply values
    (let loop ([s sig] [n len])
      (if (positive? n)
        (cons (signal-first^ s)
              (loop (signal-rest^ s) (sub1 n)))
        empty))))

; Convert a vector of signals into a signal of vectors.
(define (signal-bundle-vector vec)
  (signal-cons
    (vector-map signal-first vec)
    (signal-bundle-vector (vector-map signal-rest vec))))
