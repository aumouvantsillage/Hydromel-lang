; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/syntax
  syntax/parse/define
  syntax/id-table)

(provide
  with-scope
  set-scope
  bind!
  lookup
  internal-name)

(struct scope (parent table))

(define current-scope (make-parameter #f))

(define-syntax-parse-rule (with-scope body ...)
  (parameterize ([current-scope (scope (current-scope) (make-free-id-table))])
    body ...))

(define (set-scope stx)
  (syntax-property stx 'scope (current-scope)))

(define (bind! name data)
  (dict-set! (scope-table (current-scope)) name data))

(define (internal-name name)
  (format-id name "~a$hydromel" name))

(define (lookup name [pred (const #t)] #:scope [sc (syntax-property name 'scope)])
  (if sc
    (dict-ref (scope-table sc) name
      (thunk (lookup name pred #:scope (scope-parent sc))))
    (let ([res (syntax-local-value (internal-name name))])
      (unless res
        (raise-syntax-error #f "No declaration found for this identifier" name))
      res)))
