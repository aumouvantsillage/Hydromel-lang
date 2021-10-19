; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "expander.rkt"
  "std.rkt"
  (for-syntax
    racket/syntax
    syntax/parse))

(provide
  (for-syntax (all-defined-out)))

(begin-for-syntax
  (define-syntax-class import
    #:literals [import]
    (pattern (import path (~optional name))))

  (define-syntax-class design-unit
    #:attributes [name (body 1)]
    (pattern :interface)
    (pattern :component))

  (define-syntax-class interface
    #:literals [interface]
    (pattern (interface name body ...)))

  (define-syntax-class component
    #:literals [component]
    (pattern (component name body ...)))

  (define-syntax-class parameter
    #:literals [parameter]
    (pattern (parameter name type)))

  (define-syntax-class data-port
    #:literals [data-port]
    (pattern (data-port name mode type)))

  (define-syntax-class composite-port
    #:literals [composite-port]
    (pattern (composite-port name (mult ...) mode:composite-mode ... intf-name arg ...)
      #:attr splice? (member 'splice (syntax->datum #'(mode ...)))
      #:attr flip?   (member 'flip   (syntax->datum #'(mode ...)))))

  (define-syntax-class composite-mode
    #:literals [flip splice]
    (pattern flip)
    (pattern splice))

  (define-syntax-class typedef
    #:literals [typedef]
    (pattern (typedef name param ... expr)))

  (define-syntax-class constant
    #:literals [constant]
    (pattern (constant name expr)))

  (define-syntax-class local-signal
    #:literals [local-signal]
    (pattern (local-signal name (~optional type) expr)))

  (define-syntax-class assignment
    #:literals [assignment]
    (pattern (assignment target expr)))

  (define-syntax-class instance
    #:literals [instance]
    (pattern (instance name (mult ...) comp-name arg ...)))

  (define-syntax-class if-statement
    #:literals [if-statement]
    (pattern (if-statement (~optional name) (~seq condition then-body) ... else-body)))

  (define-syntax-class for-statement
    #:literals [for-statement]
    (pattern (for-statement (~optional name) iter-name iter-expr body)))

  (define-syntax-class statement-block
    #:literals [statement-block]
    (pattern (statement-block body ...)))

  (define-syntax-class literal-expr
    #:literals [literal-expr]
    (pattern (literal-expr value)))

  (define-syntax-class name-expr
    #:literals [name-expr]
    (pattern (name-expr name (~optional suffix))))

  (define-syntax-class field-expr
    #:literals [field-expr]
    (pattern (field-expr expr field-name (~optional type-name))))

  (define-syntax-class indexed-port-expr
    #:literals [indexed-port-expr]
    (pattern (indexed-port-expr expr index ...)))

  (define-syntax-class register-expr
    #:literals [register-expr]
    (pattern (register-expr init-expr   (~optional init-cond:when-clause)
                            update-expr (~optional update-cond:when-clause))))

  (define-syntax-class comprehension
    (pattern (mode:comprehension-mode body (~seq iter-name iter-expr) ...+)))

  (define-syntax-class comprehension-mode
    #:literals [array-for-expr concat-for-expr]
    (pattern array-for-expr)
    (pattern concat-for-expr))

  (define-syntax-class when-clause
    #:literals [when-clause]
    (pattern (when-clause expr)))

  (define-syntax-class call-expr
    #:literals [call-expr or-expr and-expr rel-expr add-expr mult-expr shift-expr
                if-expr prefix-expr range-expr slice-expr concat-expr array-expr
                indexed-array-expr]
    (pattern ((~or* or-expr and-expr rel-expr add-expr mult-expr shift-expr) left op right)
      #:attr (arg 1) (list #'left #'right)
      #:attr fn-name (format-id #'op "&~a" #'op))
    (pattern (prefix-expr op right)
      #:attr (arg 1) (list #'right)
      #:attr fn-name (format-id #'op "&~a" #'op))
    (pattern (if-expr arg ...)
      #:attr fn-name #'&if)
    (pattern (range-expr left op right)
      #:attr (arg 1) (list #'left #'right)
      #:attr fn-name #'&range)
    (pattern (slice-expr expr (range-expr left op right))
      #:attr (arg 1) (list #'expr #'left #'right)
      #:attr fn-name #'slice)
    (pattern (slice-expr expr index)
      #:attr (arg 1) (list #'expr #'index #'index)
      #:attr fn-name #'slice)
    (pattern (indexed-array-expr expr index)
      #:attr (arg 1) (list #'expr #'index)
      #:attr fn-name #'array-ref)
    (pattern (concat-expr arg ...)
      #:attr fn-name #'concat)
    (pattern (array-expr arg ...)
      #:attr fn-name #'make-array)
    (pattern (call-expr fn-name arg ...)))

  (define-syntax-class lift-expr
    #:literals [lift-expr]
    (pattern (lift-expr binding ...+ expr))))

; TODO Add types, type expressions, functions
