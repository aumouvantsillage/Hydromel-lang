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
  define-return-type
  current-call-expr)

(begin-for-syntax
  (define (function-return-type-name name)
    (format-id name "~a:return-type" name)))

(define-syntax-parse-rule (declare-function name)
  (declare-function* no-cast name))

(define-syntax-parse-rule (declare-function/cast name)
  (declare-function* cast name))

(define-syntax-parse-rule (define-function arg ...)
  (define-function* no-cast arg ...))

(define-syntax-parse-rule (define-function/cast arg ...)
  (define-function* cast arg ...))

(define-syntax-parser declare-function*
  #:datum-literals [cast no-cast]
  [(_ no-cast name)
   #:with lookup-name (internal-name #'name)
   #:with rt-name     (function-return-type-name #'name)
   #'(begin
       (provide name lookup-name rt-name)
       (define-syntax lookup-name (meta/make-function #f #'name)))]
  [(_ cast name)
   #:with lookup-name (internal-name #'name)
   #:with rt-name     (function-return-type-name #'name)
   #'(begin
       (provide name lookup-name rt-name)
       (define-syntax lookup-name (meta/make-function/cast #f #'name)))])

(define-syntax-parser define-function*
  [(_ cast? name fn rt-fn)
   #:with rt-name (function-return-type-name #'name)
   #'(begin
       (declare-function* cast? name)
       (define name fn)
       (define rt-name (return-type-function cast? name rt-fn)))]

  [(_ cast? name fn)
   #'(define-function* cast? name fn (const (union-type empty)))])

(define current-call-expr (make-parameter #f))

(define-syntax-parser return-type-function
  #:literals [λ const]
  #:datum-literals [cast no-cast]
  [(_ no-cast name (λ (arg:id ...) body ...))
   #'(λ (stx arg ...)
       (define t (parameterize ([current-call-expr stx])
                   body ...))
       (if (and (const-type? arg) ...)
         (make-const-type (name (const-type-value arg) ...))
         t))]

  [(_ cast name (λ (arg:id ...) body ...))
   #'(λ (stx arg ...)
       (define t (parameterize ([current-call-expr stx])
                   body ...))
       (if (and (const-type? arg) ...)
         (make-const-type (t (name (const-type-value arg) ...)))
         t))]

  [(_ no-cast name (λ args:id body ...))
   #'(λ (stx . args)
       (define t (parameterize ([current-call-expr stx])
                   body ...))
       (if (andmap const-type? args)
         (make-const-type (apply name (map const-type-value args)))
         t))]

  [(_ cast name (λ args:id body ...))
   #'(λ (stx . args)
       (define t (parameterize ([current-call-expr stx])
                   body ...))
       (if (andmap const-type? args)
         (make-const-type (t (apply name (map const-type-value args))))
         t))]

  [(_ cast? name (const body ...))
   #'(return-type-function cast? name (λ args body ...))]

  [(_ cast? name (fn-name:id arg ...))
   #'(return-type-function cast? name (λ (arg ...) (fn-name arg ...)))])

(define-syntax-parse-rule (define-return-type name fn)
  #:with rt-name (function-return-type-name #'name)
  (define rt-name (λ (stx . args)
                    (parameterize ([current-call-expr stx])
                      (apply fn args)))))
