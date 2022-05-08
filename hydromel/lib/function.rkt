; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  (for-syntax
    (prefix-in meta/ "meta.rkt")
    racket/syntax
    "scope.rkt")
  "types.rkt")

(provide
  declare-function      define-function
  declare-function/cast define-function/cast
  define-function/return-type
  (for-syntax function-return-type-name))

(begin-for-syntax
  ; Format an internal function name for a return-type function.
  (define (function-return-type-name name)
    (format-id name "~a$return-type" name)))

; Declare a global function that does not need explicit casting of its result.
; `name` is the name of a function that must be defined separately.
; Use `define-function/return-type` to define the function that computes the return
; type of this function.
(define-syntax-parse-rule (declare-function name)
  (declare-function* #f name))

; Declare a global function that needs explicit casting of its result.
; `name` is the name of a function that must be defined separately.
; Use `define-function/return-type` to define the function that computes the return
; type of this function.
(define-syntax-parse-rule (declare-function/cast name)
  (declare-function* #t name))

; Define a global function that does not need explicit casting of its result.
; `name` is the name of the new function.
; `fn ...` are one or two functions that implement the new function
; and its return type.
(define-syntax-parse-rule (define-function name fn ...+)
  (define-function* #f name fn ...))

; Define a global function that needs explicit casting of its result.
; `name` is the name of the new function.
; `fn ...` are one or two functions that implement the new function
; and its return type.
(define-syntax-parse-rule (define-function/cast name fn ...+)
  (define-function* #t name fn ...))

; Declare a global function.
; This macro binds function metadata to an internal name in the global scope.
(define-syntax-parse-rule (declare-function* cast? name)
   #:with lookup-name (internal-name #'name)
   #:with rt-name     (function-return-type-name #'name)
   (begin
     (provide name lookup-name rt-name)
     (define-syntax lookup-name (meta/function #f #'name cast?))))

; Define a global function.
; This macro declares the function with `declare-function*`.
(define-syntax-parse-rule (define-function* cast? name fn (~optional rt-fn))
   #:with rt-name (function-return-type-name #'name)
   #:with rt-fn^ (or (attribute rt-fn) #'(const (union-type empty)))
   (begin
     (declare-function* cast? name)
     (define name fn)
     (define rt-name (make-return-type-function cast? name rt-fn^))))

; Wrap a return type function `fn` provided to `define-function`.
; The resulting function will set the current syntax object for type-checking
; errors. It will also convert the result of `fn` to a `const-type` if all
; arguments are constants.
(define (make-return-type-function cast? name fn)
  (λ (stx . args)
    (define t (parameterize ([current-typecheck-stx stx])
                (apply fn args)))
    (if (andmap const-type? args)
      (let ([v (apply name (map const-type-value args))])
        (make-const-type (if cast? (t v) v)))
      t)))

; Wrap a return type function `fn` for a function declared previously with
; `declare-function`. The resulting function will set the current syntax object
; for type-checking errors. It does NOT check whether its arguments are constants.
; `fn` is responsible for wrapping its result in a `const-type` if appropriate.
(define-syntax-parse-rule (define-function/return-type name fn)
  #:with rt-name (function-return-type-name #'name)
  (begin
    (provide rt-name)
    (define rt-name (λ (stx . args)
                      (parameterize ([current-typecheck-stx stx])
                        (apply fn args))))))
