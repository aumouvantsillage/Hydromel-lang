; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/syntax
  syntax/parse/define
  syntax/id-table
  "errors.rkt")

(provide
  with-scope
  set-scope
  bind!
  lookup
  internal-name)

; A scope contains a reference to a parent scope and a dictionary that
; maps names to metadata.
; A scope instance corresponds to a local scope in a Hydromel element such as
; an interface or a component. The global scope is managed by Racket.
; If `parent` is false, then the parent scope is the global scope.
(struct scope (parent table))

; A parameter that contains a reference to the current local scope during the
; binding phase of the semantic checker (see function `add-scopes` in checker.rkt).
; Use the `with-scope` macro to set this parameter.
(define current-scope (make-parameter #f))

; Execute the given body in the context of a new local scope.
; This macro has the following effect:
; 1. Create an empty `scope` as a child of the current scope.
; 2. Set the parameter `current-scope` to the new scope.
; 3. Execute `body`.
; 4. Restore the previous current scope.
(define-syntax-parse-rule (with-scope body ...)
  (parameterize ([current-scope (scope (current-scope) (make-free-id-table))])
    body ...))

; Attach the current scope as a syntax property to the given syntax object.
(define (set-scope stx)
  (syntax-property stx 'scope (current-scope)))

; Add an entry to the current scope.
(define (bind! name data)
  (dict-set! (scope-table (current-scope)) name data))

; Create an internal name for an element in the global scope.
(define (internal-name name)
  (format-id name "~a$hydromel" name))

; Lookup the data associated with the given name in the given scope.
; By default, the scope is the `scope` syntax property attached to the name.
; The lookup process will traverse the scope hierarchy from child to parent
; until it reaches the global scope.
; If the given name was not found, a semantic error is raised.
(define (lookup name #:scope [sc (syntax-property name 'scope)])
  (if sc
    (dict-ref (scope-table sc) name
      (thunk (lookup name #:scope (scope-parent sc))))
    (syntax-local-value (internal-name name)
      (thunk (raise-semantic-error "No declaration found for identifier" name)))))
