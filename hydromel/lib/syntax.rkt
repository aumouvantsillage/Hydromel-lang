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

  (define-syntax-class choices
    #:literals [choices]
    (pattern (choices expr ...)))

  (define-syntax-class call-expr/cast
    #:literals [call-expr/cast]
    (pattern (call-expr/cast fn-name arg ...)))

  (define-syntax-class array-assoc
    #:literals [array-assoc]
    (pattern (array-assoc index ... expr)
      #:attr (arg 1) (list #'(tuple-expr index ...) #'expr)))

  (define-syntax-class slice-assoc
    #:literals [slice-assoc range-expr]
    (pattern (slice-assoc (range-expr left op right) expr)
      #:attr (arg 1) (list #'left #'right #'expr))
    (pattern (slice-assoc index expr)
      #:attr (arg 1) (list #'index #'index #'expr)))

  (define-syntax-class call-expr
    #:literals [call-expr or-expr and-expr rel-expr add-expr mult-expr shift-expr
                if-expr case-expr prefix-expr range-expr slice-expr concat-expr
                array-expr array-assoc-expr slice-assoc-expr indexed-array-expr
                field-expr cast-expr assign-expr record-type-expr record-expr tuple-expr]
    #:attributes [fn-name (arg 1)]
    (pattern ((~or* or-expr and-expr rel-expr add-expr mult-expr shift-expr) left op right)
      #:attr (arg 1) (list #'left #'right)
      #:attr fn-name (format-id #'op "_~a_" #'op))
    (pattern (prefix-expr (~datum -) right)
      #:attr (arg 1) (list #'right)
      #:attr fn-name #'_neg_)
    (pattern (prefix-expr op right)
      #:attr (arg 1) (list #'right)
      #:attr fn-name (format-id #'op "_~a_" #'op))
    (pattern (if-expr arg ...)
      #:attr fn-name #'_if_)
    (pattern (case-expr arg ...)
      #:attr fn-name #'_case_)
    (pattern (cast-expr arg ...)
      #:attr fn-name #'_cast_)
    (pattern (range-expr left op right)
      #:attr (arg 1) (list #'left #'right)
      #:attr fn-name #'_range_)
    (pattern (slice-expr expr (range-expr left op right))
      #:attr (arg 1) (list #'expr #'left #'right)
      #:attr fn-name #'_slice_)
    (pattern (slice-expr expr index)
      #:attr (arg 1) (list #'expr #'index #'index)
      #:attr fn-name #'_slice_)
    (pattern (indexed-array-expr arg ...)
      #:attr fn-name #'_nth_)
    (pattern (assign-expr left (array-assoc-expr nv:array-assoc ...))
      #:attr (arg 1) (cons #'left (apply append (attribute nv.arg)))
      #:attr fn-name #'_set_nth_)
    (pattern (assign-expr left (slice-assoc-expr lrv:slice-assoc ...))
      #:attr (arg 1) (cons #'left (apply append (attribute lrv.arg)))
      #:attr fn-name #'_set_slice_)
    (pattern (assign-expr left (record-expr (~seq field-name field-value) ...))
      #:with (field ...) #'((~@ (literal-expr field-name) field-value) ...)
      #:attr (arg 1) (cons #'left (attribute field))
      #:attr fn-name #'_set_field_)
    (pattern (concat-expr arg ...)
      #:attr fn-name #'_concat_)
    (pattern (array-expr arg ...)
      #:attr fn-name #'_array_)
    (pattern (record-type-expr (~seq field-name field-type) ...)
      #:with (field ...) #'((~@ (literal-expr field-name) field-type) ...)
      #:attr fn-name #'record
      #:attr (arg 1) (attribute field))
    (pattern (record-expr (~seq field-name field-value) ...)
      #:with (field ...) #'((~@ (literal-expr field-name) field-value) ...)
      #:attr fn-name #'_record_
      #:attr (arg 1) (attribute field))
    (pattern (tuple-expr arg ...)
      #:attr fn-name #'_tuple_)
    (pattern (call-expr fn-name arg ...))
    (pattern :call-expr/cast))
    ; field-expr is handled in checker.rkt

  (define-syntax-class lift-expr
    #:literals [lift-expr]
    (pattern (lift-expr binding ...+ expr))))

; TODO Add functions
