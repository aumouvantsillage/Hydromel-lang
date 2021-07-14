#lang racket

(require
  "expander.rkt"
  (for-syntax
    syntax/parse))

(provide
  (for-syntax (all-defined-out)))

(begin-for-syntax
  (define-syntax-class use
    #:literals [use]
    (pattern (use path:str)))

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
    #:literals [composite-port multiplicity]
    (pattern (composite-port name (~optional (multiplicity mult)) mode:composite-mode ... intf-name arg ...)
      #:attr splice? (member 'splice (syntax->datum #'(mode ...)))
      #:attr flip?   (member 'flip   (syntax->datum #'(mode ...)))))

  (define-syntax-class composite-mode
    #:literals [flip splice]
    (pattern flip)
    (pattern splice))

  (define-syntax-class constant
    #:literals [constant]
    (pattern (constant name expr)))

  (define-syntax-class local-signal
    #:literals [local-signal]
    (pattern (local-signal name expr)))

  (define-syntax-class assignment
    #:literals [assignment]
    (pattern (assignment target expr)))

  (define-syntax-class instance
    #:literals [instance multiplicity]
    (pattern (instance name (~optional (multiplicity mult)) comp-name arg ...)))

  (define-syntax-class literal-expr
    #:literals [literal-expr]
    (pattern (literal-expr value)))

  (define-syntax-class name-expr
    #:literals [name-expr]
    (pattern (name-expr name)))

  (define-syntax-class field-expr
    #:literals [field-expr]
    (pattern (field-expr expr field-name (~optional type-name))))

  (define-syntax-class indexed-expr
    #:literals [indexed-expr]
    (pattern (indexed-expr expr index ...)))

  (define-syntax-class register-expr
    #:literals [register-expr]
    (pattern (register-expr init-expr (~optional init-cond:when-clause)
                            update-expr (~optional update-cond:when-clause))))

  (define-syntax-class when-clause
    #:literals [when-clause]
    (pattern (when-clause expr)))

  (define-syntax-class call-expr
    #:literals [call-expr]
    #:datum-literals [or-expr and-expr rel-expr add-expr mult-expr if-expr prefix-expr]
    (pattern ((~or* or-expr and-expr rel-expr add-expr mult-expr) left fn-name right)
      #:attr (arg 1) (list #'left #'right))
    (pattern (prefix-expr fn-name right)
      #:attr (arg 1) (list #'right))
    (pattern (if-expr arg ...)
      #:attr fn-name 'if)
    (pattern (call-expr fn-name arg ...)))

  (define-syntax-class alias
    #:literals [alias]
    (pattern (alias name port-name intf-name)))

  (define-syntax-class lift-expr
    #:literals [lift-expr]
    (pattern (lift-expr binding ...+ expr))))
