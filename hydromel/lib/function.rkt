; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  (for-syntax
    (prefix-in meta/ "meta.rkt")
    racket/syntax)
  (only-in "types.rkt"
    static-data/literal
    static-data?
    static-data-value
    none))

(provide
  define-function
  define-function*
  define-function/cast
  define-function*/cast)

(begin-for-syntax
  (define (function-impl-name name)
    (format-id name "~a:impl" name))

  (define (function-return-type-name name)
    (format-id name "~a:return-type" name)))

(define-syntax-parse-rule (declare-function name fn-name)
  (begin
    (provide name)
    (define-syntax name (meta/make-function #'fn-name))))

(define-syntax-parse-rule (declare-function/cast name fn-name)
  (begin
    (provide name)
    (define-syntax name (meta/make-function/cast #'fn-name))))

(define-syntax-parse-rule (define-function* name)
  #:with fn-name (function-impl-name #'name)
  #:with rt-name (function-return-type-name #'fn-name)
  (begin
    (declare-function name fn-name)
    (provide fn-name rt-name)))

(define-syntax-parse-rule (define-function*/cast name)
  #:with fn-name (function-impl-name #'name)
  #:with rt-name (function-return-type-name #'fn-name)
  (begin
    (declare-function/cast name fn-name)
    (provide fn-name rt-name)))

(define-syntax-parse-rule (define-function arg ...)
  (do-define-function no-cast arg ...))

(define-syntax-parse-rule (define-function/cast arg ...)
  (do-define-function cast arg ...))

(define-syntax-parser do-define-function
  #:datum-literals [cast no-cast]
  [(_ no-cast name fn-name:id rt-fn)
   #'(begin
       (declare-function name fn-name)
       (define-return-type no-cast fn-name rt-fn))]

  [(_ cast name fn-name:id rt-fn)
   #'(begin
       (declare-function/cast name fn-name)
       (define-return-type cast fn-name rt-fn))]

  [(_ cast? name fn rt-fn)
   #:with fn-name (function-impl-name #'name)
   #'(begin
       (provide fn-name)
       (define fn-name fn)
       (do-define-function cast? name fn-name rt-fn))]

  [(_ cast? name fn)
   #'(do-define-function cast? name fn (const (none)))])

(define-syntax-parse-rule (define-return-type cast? name fn)
  #:with rt-name (function-return-type-name #'name)
  (begin
      (provide rt-name)
      (define rt-name (return-type-function cast? name fn))))

(define-syntax-parser return-type-function
  #:literals [λ const]
  #:datum-literals [cast no-cast]
  [(_ no-cast name (λ (arg:id ...) body ...))
   #'(λ (arg ...)
       (if (and (static-data? arg) ...)
         (static-data/literal (name (static-data-value arg) ...))
         (let () body ...)))]

  [(_ cast name (λ (arg:id ...) body ...))
   #'(λ (arg ...)
       (define t (let () body ...))
       (if (and (static-data? arg) ...)
         (static-data/literal (t (name (static-data-value arg) ...)))
         t))]

  [(_ no-cast name (λ args:id body ...))
   #'(λ args
       (if (andmap static-data? args)
         (static-data/literal (apply name (map static-data-value args)))
         (let () body ...)))]

  [(_ cast name (λ args:id body ...))
   #'(λ args
       (define t (let () body ...))
       (if (andmap static-data? args)
         (static-data/literal (t (apply name (map static-data-value args))))
         t))]

  [(_ cast? name (const body ...))
   #'(return-type-function cast? name (λ args body ...))]

  [(_ cast? name (fn-name:id arg ...))
   #'(return-type-function cast? name (λ (arg ...) (fn-name arg ...)))])
