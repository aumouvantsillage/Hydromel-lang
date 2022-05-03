; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  "expander.rkt"
  "std.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    racket
    threading
    racket/function
    racket/syntax
    syntax/parse
    syntax/parse/define
    "errors.rkt"
    "scope.rkt"
    (prefix-in meta/ "meta.rkt")))

(provide begin-hydromel)

(define-syntax-parse-rule (begin-hydromel body ...)
  (begin
    (compile-as-module-level-defs body) ...
    (compile-hydromel body ...)))

; First pass: bind module-level elements and manage module imports.
(define-syntax-parser compile-as-module-level-defs
  [(_ s:stx/import)
   (if (attribute s.name)
     (let ([prefix (format-id #'s.name "~a::" #'s.name)])
       #`(require (prefix-in #,prefix s.path)))
     #'(require s.path))]

  [(_ s:stx/typedef)
   #:with lookup-name (internal-name #'s.name)
   #'(begin
       (provide lookup-name)
       (define-syntax lookup-name (meta/make-function #'s #'s.name)))]

  [(_ s:stx/constant)
   #:with lookup-name (internal-name #'s.name)
   #'(begin
       (provide lookup-name)
       (define-syntax lookup-name (meta/constant #'s #t)))]

  [(_ s:stx/interface)
   #:with lookup-name (internal-name #'s.name)
   #:with (p ...) (design-unit-field-metadata (attribute s.body))
   #'(begin
       (provide lookup-name)
       (define-syntax lookup-name (meta/make-interface #'s p ...)))]

  [(_ s:stx/component)
   #:with lookup-name (internal-name #'s.name)
   #:with (p ...) (design-unit-field-metadata (attribute s.body))
   #'(begin
       (provide lookup-name)
       (define-syntax lookup-name (meta/make-component #'s p ...)))]

  [_
   #'(begin)])

; Second pass: create the scope hierarchy and bindings to local metadata.
; Third pass:  perform semantic checking.
; Fourth pass: add labels for the type checker.
(define-syntax (compile-hydromel stx)
  (~> stx add-scopes check label))

(begin-for-syntax
  (define (boolean->syntax b)
    (if b #'#t #'#f))

  ; Short forms for syntax/loc inside syntax-parse.
  (define-syntax-parse-rule (s/l expr)
    (syntax/loc this-syntax expr))

  ; Short forms for quasyntax/loc inside syntax-parse.
  (define-syntax-parse-rule (q/l expr)
    (quasisyntax/loc this-syntax expr))

  ; First pass: fill the metadata of module-level elements.
  ; Those metadata are constructed as syntax objects that will be inserted
  ; into the result of compile-as-module-level-defs.
  (define (design-unit-field-metadata lst)
    (filter identity
      (for/list ([stx (in-list lst)])
        (syntax-parse stx
          [s:stx/data-port
           (s/l (cons 's.name (meta/data-port #'s 's.mode)))]

          [s:stx/composite-port
           #:with flip   (boolean->syntax (attribute s.flip?))
           #:with splice (boolean->syntax (attribute s.splice?))
           (s/l (cons 's.name (meta/composite-port #'s #'s.intf-name flip splice)))]

          [s:stx/constant
           (s/l (cons 's.name (meta/constant #'s #f)))]

          [_ #f]))))

  ; ----------------------------------------------------------------------------
  ; Scoping.
  ; ----------------------------------------------------------------------------

  (define current-design-unit (make-parameter #f))

  (define (add-scopes stx)
    (syntax-parse stx
      [s:stx/design-unit
       #:with (unit-type _ ...) this-syntax
       #:with (body ...) (parameterize ([current-design-unit (lookup #'s.name)])
                           (with-scope
                             (~>> (attribute s.body)
                                  create-aliases
                                  (map add-scopes))))
       (s/l (unit-type s.name body ...))]

      [s:stx/parameter
       (bind! #'s.name (meta/parameter this-syntax))
       (add-scopes* this-syntax)]

      [s:stx/typedef
       (when (current-design-unit)
         (bind! #'s.name (meta/make-function this-syntax #'s.name)))
       (with-scope (add-scopes* this-syntax))]

      [s:stx/constant #:when (current-design-unit)
       (bind! #'s.name (meta/design-unit-ref (current-design-unit) #'s.name))
       (add-scopes* this-syntax)]

      [s:stx/data-port
       (bind! #'s.name (meta/design-unit-ref (current-design-unit) #'s.name))
       (add-scopes* this-syntax)]

      [s:stx/composite-port
       (bind! #'s.name (meta/design-unit-ref (current-design-unit) #'s.name))
       (add-scopes* this-syntax)]

      [s:stx/instance
       (bind! #'s.name (meta/instance this-syntax #'s.comp-name))
       (add-scopes* this-syntax)]

      [s:stx/local-signal
       (bind! #'s.name (meta/local-signal this-syntax))
       (add-scopes* this-syntax)]

      [s:stx/for-statement
       #:with name (or (attribute s.name) (generate-temporary #'for))
       #:with iter-expr (add-scopes #'s.iter-expr)
       ; The loop counter is bound as a constant inside a new scope
       ; so that only the loop body can use it.
       #:with body (with-scope
                     (bind! #'s.iter-name (meta/constant this-syntax #f))
                     (add-scopes #'s.body))
       (s/l (for-statement name s.iter-name iter-expr body))]

      [s:stx/statement-block
       #:with (body ...) (with-scope (map add-scopes (attribute s.body)))
       (s/l (statement-block body ...))]

      [s:stx/comprehension
       ; Loop counters are bound as constants inside a new scope.
       ; The actual values of these constants do not matter.
       ; Range expressions are allowed to depend on other loop counters.
       #:with (body iter-expr ...) (with-scope
                                     (for ([name (in-list (attribute s.iter-name))])
                                       (bind! name (meta/constant this-syntax #f)))
                                     (map add-scopes (cons #'s.body (attribute s.iter-expr))))
       (s/l (s.mode body (~@ s.iter-name iter-expr) ...))]

      ; Attach the current scope as a syntax property to the current name,
      ; allowing to perform lookups in the semantic checking pass.
      [s:stx/name-expr
       #:with name (set-scope #'s.name)
       (s/l (name-expr name))]

      [_ (add-scopes* this-syntax)]))

  (define (add-scopes* stx)
    (syntax-parse stx
      ; Add scopes recursively to any list form that
      ; does not create bindings or introduce a new scope.
      [(item ...)
       #:with (item^ ...) (map add-scopes (attribute item))
       (s/l (item^ ...))]

      [_ this-syntax]))

  ; ----------------------------------------------------------------------------
  ; Semantic checking.
  ; ----------------------------------------------------------------------------

  (define (check stx)
    (syntax-parse stx
      #:literals [compile-hydromel design-unit]

      [(compile-hydromel body ...)
       #:with (body^ ...) (map check (attribute body))
       (s/l (begin body^ ...))]

      [s:stx/import
       #'(begin)]

      [s:stx/design-unit
       #:with (body ...) (map check (attribute s.body))
       (s/l (design-unit s.name body ...))]

      [s:stx/parameter
       #:with type (check #'s.type)
       (s/l (parameter s.name type))]

      [s:stx/typedef
       #:with (param ...) (map check (attribute s.param))
       #:with expr        (check #'s.expr)
       (s/l (typedef s.name param ... expr))]

      [s:stx/constant
       #:with expr (check #'s.expr)
       ; Check that the expression has a static value.
       (unless (static? #'expr)
         (raise-semantic-error "Non-static expression cannot be assigned to constant" this-syntax #'s.expr))
       (s/l (constant s.name expr))]

      [s:stx/data-port
       #:with type (check #'s.type)
       (s/l (data-port s.name s.mode type))]

      [s:stx/composite-port
       #:with (mult ...) (map check (attribute s.mult))
       #:with (arg ...) (map check (attribute s.arg))
       ; Check that the multiplicity has a static value.
       (for ([m (in-list (attribute mult))])
         (unless (static? m)
           (raise-semantic-error "Non-static expression cannot be used as port multiplicity" this-syntax m)))
       ; Check that intf-name refers to an existing interface
       (meta/lookup-interface this-syntax #'s.intf-name)
       ; Check arguments
       (s/l (composite-port s.name (mult ...) s.mode ... s.intf-name arg ...))]

      [s:stx/instance
       #:with (mult ...) (map check (attribute s.mult))
       #:with (arg ...) (map check (attribute s.arg))
       ; Check that the multiplicity has a static value.
       (for ([m (in-list (attribute mult))])
         (unless (static? m)
           (raise-semantic-error "Non-static expression cannot be used as instance multiplicity" this-syntax m)))
       ; Check that comp-name refers to an existing component
       (meta/lookup-component this-syntax #'s.comp-name)
       (s/l (instance s.name (mult ...) s.comp-name arg ...))]

      [s:stx/local-signal
       #:with expr (check-assigned-expr (check #'s.expr))
       (if (attribute s.type)
         (syntax-parse (check #'s.type)
           [type (s/l (local-signal s.name type expr))])
         (s/l (local-signal s.name expr)))]

      [s:stx/assignment
       ; TODO check circular dependencies.
       #:with target (check #'s.target)
       #:with expr   (check #'s.expr)
       (define target-port (check-assignment-target this-syntax #'target))
       (if (meta/composite-port? target-port)
         ; If the left-hand side is a composite port,
         ; generate an assignment for each data port.
         (let ([expr-port (resolve #'expr)])
           (unless (meta/composite-port? expr-port)
             (raise-semantic-error "Right-hand side of assignment is not a composite port" this-syntax #'expr))
           (unless (equal? (syntax-e (meta/composite-port-intf-name target-port))
                           (syntax-e (meta/composite-port-intf-name expr-port)))
             (raise-semantic-error "Right-hand side and left-hand side of assignment have different interfaces" #'s))
           ; Here we pass s.target and s.expr because they will be checked again.
           (check-composite-assignment stx #'s.target #'s.expr target-port))
         ; If the left-hand side is a signal, generate an assignment statement.
         (q/l (assignment target #,(check-assigned-expr #'expr))))]

      [s:stx/if-statement
       #:with name            (or (attribute s.name) (generate-temporary #'if))
       #:with (condition ...) (map check (attribute s.condition))
       #:with (then-body ...) (map check (attribute s.then-body))
       #:with else-body       (if (attribute s.else-body)
                                (check #'s.else-body)
                                #'(statement-block))
       (for ([it (in-list (attribute condition))]
             #:unless (static? it))
         (raise-semantic-error "Non-static expression cannot be used as condition in if statement" this-syntax it))
       (s/l (if-statement name (~@ condition then-body) ... else-body))]

      [s:stx/for-statement
       #:with iter-expr (check #'s.iter-expr)
       #:with body (check #'s.body)
       (unless (static? #'iter-expr)
         (raise-semantic-error "Non-static expression cannot be used as loop range" this-syntax #'iter-expr))
       (q/l (for-statement s.name s.iter-name iter-expr body))]

      [s:stx/statement-block
       #:with (body ...) (map check (attribute s.body))
       (s/l (statement-block body ...))]

      [s:stx/field-expr
       #:with expr (check #'s.expr)
       (if (meta/design-unit? (check-field-expr this-syntax #'expr #'s.field-name))
         (s/l (field-expr expr s.field-name))
         ; We use s.expr here because check fails if we
         ; check an already checked call-expr.
         (check (s/l (call-expr _field_ s.expr (literal-expr s.field-name)))))]

      [s:stx/indexed-port-expr
       #:with expr (check #'s.expr)
       #:with (index ...) (map check (attribute s.index))
       (define r (resolve #'expr))
       (unless (or (meta/composite-port? r) (meta/instance? r))
         (raise-semantic-error "Expression is neither a composite port nor an instance" this-syntax #'s.expr))
       ; TODO Report semantic error if r doesn't have a multiplicity.
       (s/l (indexed-port-expr expr index ...))]

      [s:stx/register-expr
       #:with init-expr (check #'s.init-expr)
       #:with (arg ...) (filter identity
                          (list
                            #'init-expr
                            (and (attribute s.init-cond) (check #'s.init-cond))
                            (check-assigned-expr (check #'s.update-expr))
                            (and (attribute s.update-cond) (check #'s.update-cond))))
       (unless (static? #'init-expr)
         (raise-semantic-error "Non-static expression cannot be used as an initial register value" this-syntax #'s.init-expr))
       (s/l (register-expr arg ...))]

      [s:stx/when-clause
       #:with expr (check-assigned-expr (check #'(call-expr int->bool s.expr)))
       (s/l (when-clause expr))]

      [s:stx/call-expr
       #:with (arg ...) (map check (attribute s.arg))
       (define fn (lookup #'s.fn-name))
       ; TODO check that fn is a built-in or custom function.
       (define/syntax-parse fn-name (if (meta/function? fn)
                                      (meta/function-name fn)
                                      #'s.fn-name))
       ; Bitwise concatenation is a special case where we interleave
       ; argument values with their inferred types.
       (define/syntax-parse (arg+ ...) (if (equal? (syntax->datum #'fn-name) '_concat_)
                                         #'((~@ arg (expression-type arg)) ...)
                                         #'(arg ...)))
       (if (and (meta/function? fn) (meta/function-cast? fn))
         (s/l (call-expr/cast fn-name arg+ ...))
         (s/l (call-expr      fn-name arg+ ...)))]

      [s:stx/name-expr
       (match (lookup #'s.name)
         ; A function name is converted to a function call with no argument.
         [(? procedure?)
          (q/l (call-expr s.name))]

         ; A function name is converted to a function call with no argument.
         [(meta/function _ fn-name _)
          (q/l (call-expr #,fn-name))]

         ; For a global constant name, append a suffix to access the constant slot.
         [(meta/constant _ #t)
          (s/l (name-expr s.name $constant))]

         [else stx])]

      [s:stx/comprehension
       #:with (iter-expr ...) (map check (attribute s.iter-expr))
       #:with body (check #'s.body)
       (for ([it (in-list (attribute iter-expr))])
         (unless (static? it)
           (raise-semantic-error "Non-static expression cannot be used as comprehension range" this-syntax it)))
       (s/l (s.mode body (~@ s.iter-name iter-expr) ...))]

      [s:stx/choices
       #:with (expr ...) (map check (attribute s.expr))
       (for ([it (in-list (attribute expr))])
         (unless (static? it)
           (raise-semantic-error "Non-static expression cannot be used as choice" this-syntax it)))
       (s/l (choices expr ...))]

      [_ this-syntax]))

  ; Fourth pass: label all expressions for the type checker.
  (define (label stx)
    (define stx^ (syntax-parse stx
                   [(op arg ...)
                    #:with (arg^ ...) (map label (attribute arg))
                    (syntax/loc stx (op arg^ ...))]
                   [_ stx]))
    (syntax-property stx^ 'label (gensym 'expr)))

  ; Returns a list of ports in interface intf after splicing.
  (define (splice-interface intf flip?)
    (apply append
      (for/list ([(field-name field) (in-dict (meta/design-unit-fields intf))])
        (cons
          (cons field-name (if flip? (meta/flip-port field) field))
          (if (and (meta/composite-port? field) (meta/composite-port-splice? field))
            (~> field
                meta/composite-port-interface
                (splice-interface (xor flip? (meta/composite-port-flip? field))))
            empty)))))

  ; Insert aliases for the spliced composite ports in the given list.
  (define (create-aliases lst)
    (apply append
      (for/list ([stx (in-list lst)])
        (cons stx
              (syntax-parse stx
                [s:stx/composite-port #:when (attribute s.splice?)
                 (define intf (meta/lookup-interface this-syntax #'s.intf-name))
                 (for/list ([(name port) (in-dict (splice-interface intf (attribute s.flip?)))])
                   (define/syntax-parse port-name (datum->syntax stx name))
                   (bind! #'port-name port)
                   #'(alias port-name s.name))]
                [_ empty])))))

  ; Check whether stx is an expression with a static value.
  ; Returns true when stx is:
  ; - a literal expression,
  ; - a name expression that refers to a constant or a parameter,
  ; - a field expression whose left-hand side has a static value,
  ; - an indexed port expression whose left-hand side and indices have static values,
  ; - an array comprehension whose body and ranges are static,
  ; - a call whose arguments have static values.
  (define (static? stx)
    (syntax-parse stx
      #:literals [expression-type]
      [(expression-type _)     #t]
      [s:stx/literal-expr      #t]
      [s:stx/name-expr         (define c (lookup #'s.name)) (or (meta/constant? c) (meta/parameter? c))]
      [s:stx/field-expr        (or  (static? #'s.expr) (meta/constant? (resolve stx)))]
      [s:stx/indexed-port-expr (and (static? #'s.expr) (andmap static? (attribute s.index)))]
      [s:stx/comprehension     (and (static? #'s.body) (andmap static? (attribute s.iter-expr)))]
      [s:stx/choices           (andmap static? (attribute s.expr))]
      [s:stx/call-expr         (andmap static? (attribute s.arg))]
      [_                       #f]))

  ; Find the metadata of the given expression result.
  (define (resolve stx)
    (syntax-parse stx
      [s:stx/name-expr
       ; For a name expression, lookup the metadata in the current scope.
       (lookup #'s.name)]

      [s:stx/field-expr
       ; Resolve a field expression that refers to a port in a composite port
       ; or to an instance. check-field-expr has already been called at this point.
       ; Accesses to record fields have already been converted to call expressions
       ; and will not reach this point.
       (define target (resolve #'s.expr))
       (define unit (match target
                      [(? meta/composite-port?) (meta/composite-port-interface target)]
                      [(? meta/instance?)       (meta/instance-component       target)]))
       (meta/design-unit-ref unit #'s.field-name)]

      [s:stx/indexed-port-expr
       ; For an indexed port expression, the metadata are those of the left-hand side.
       (resolve #'s.expr)]

      [_ #f]))

  (define (flip? expr [res #f])
    (syntax-parse expr
      [s:stx/field-expr
       (define res^ (flip? #'s.expr res))
       (match (resolve #'s.expr)
         [(meta/composite-port _ _ f? _) (xor f? res^)]
         [(meta/instance       _ _)      (not res^)])]
      [_ res]))

  ; Check an expression that appears in the left-hand side of an assignment.
  (define (check-assignment-target parent-stx stx)
    (define target (resolve stx))
    (match target
      ; If the left-hand side of an assignment resolves to a data port,
      ; check the mode of this port.
      [(meta/data-port _ mode)
       (unless (equal? mode (if (flip? stx) 'in 'out))
         (raise-semantic-error "Port cannot be assigned" parent-stx stx))]

      [(meta/composite-port _ intf-name flip? splice?)
       (void)]

      [_
       (raise-semantic-error "Expression not suitable as assignment target" parent-stx stx)])
    target)

  ; Check an expression that constitutes the right-hand side of an assignment.
  ; This includes:
  ; - the expression assigned to a local signal,
  ; - the update clause of a register expression,
  ; - the when clauses of a register expression.
  ; Returns an expression wrapped in lift-expr, signal-expr, or slot-expr.
  (define (check-assigned-expr stx)
    (syntax-parse (lift-if-needed stx)
      [s:stx/lift-expr
       ; If a lift-expr wraps a signal read, wrap it also in a slot-expr.
       (if (meta/signal? (resolve #'s.expr))
         (s/l (lift-expr s.binding ... (slot-expr s.expr)))
         this-syntax)]

      [_
       ; If stx has a static value, wrap it in a signal-expr.
       ; If stx resolves to a signal, wrap it in a slot-expr.
       (cond [(static? this-syntax)                (q/l (signal-expr #,this-syntax))]
             [(meta/signal? (resolve this-syntax)) (q/l (slot-expr   #,this-syntax))]
             [else                                 this-syntax])]))

  (define (lift-if-needed stx)
    (syntax-parse stx
      #:literals [indexed-port-expr field-expr lift-expr]
      [(indexed-port-expr expr ...)
       (lift-if-needed* stx (attribute expr)
         (λ (lst) (q/l (indexed-port-expr #,@lst))))]

      [(field-expr expr sel ...)
       (lift-if-needed* stx (list #'expr)
         (λ (lst) (q/l (field-expr #,(first lst) sel ...))))]

      [s:stx/comprehension
       ; If a comprehension needs lifting, we force the lift-expr form
       ; back into the comprehension body.
       ; This is useful if the bindings of the lift-expr depend on the
       ; loop counters of the comprehension.
       (syntax-parse (lift-if-needed* stx (list #'s.body)
                       (λ (lst) (q/l (s.mode #,(first lst) (~@ s.iter-name s.iter-expr) ...))))
         [(lift-expr binding ... (mode body iter ...))
          (q/l (mode (lift-expr binding ... body) iter ...))]
         [_ this-syntax])]

      [s:stx/call-expr/cast
       (lift-if-needed* stx (attribute s.arg)
         (λ (lst) (q/l (call-expr/cast s.fn-name #,@lst))))]

      [s:stx/call-expr
       (lift-if-needed* stx (attribute s.arg)
         (λ (lst) (q/l (call-expr s.fn-name #,@lst))))]

      [_
       stx]))

  (define (lift-if-needed* stx lst thunk)
    (for/foldr ([b-lst empty] ; The list of bindings for the new lift-expr
                [a-lst empty] ; The list of arguments for the new call-expr
                ; If there are no bindings, return the original call-expr,
                ; else, make a new call-expr wrapped in a lift-expr.
                #:result (if (empty? b-lst)
                           stx
                           (quasisyntax/loc stx
                             (lift-expr #,@b-lst #,(thunk a-lst)))))
                ; Lift each argument if needed before proceeding.
               ([a (in-list (map lift-if-needed lst))])
      (syntax-parse a
        [s:stx/lift-expr
         ; If the current argument is already lifted,
         ; accumulate its bindings and unwrap its expression.
         (values (append (attribute s.binding) b-lst)
                 (cons   #'s.expr              a-lst))]

        [s #:when (meta/signal? (resolve a))
         ; If the argument resolves to a signal, wrap it in a slot-expr,
         ; create a binding and replace it with a name-expr.
         #:with bname (gensym "lift")
         ; Trick: we set the location of the generated expressions so that they
         ; are correctly identified by the type checker.
         (values (cons #`(bname #,(s/l (slot-expr s))) b-lst)
                 (cons (s/l (name-expr bname))         a-lst))]

        [_
         ; In the other cases, keep the current list of bindings
         ; and the current argument.
         (values b-lst (cons a a-lst))])))

  (define (check-field-expr parent-expr expr field-name)
    (define target (resolve expr))
    (match target
      [(meta/composite-port _ intf-name _ _)
       ; We first need to check that intf-name refers to an interface, in case
       ; it has not been checked already (for instance if the port is declared later).
       (define intf (meta/composite-port-interface target)) ; TODO get syntax object of composite port
       ; Check that a port with that name exists in the interface.
       (meta/design-unit-ref intf field-name
         (thunk (raise-semantic-error (format "Port not found in interface ~a" (syntax-e intf-name)) parent-expr field-name)))
       ; Return the interface.
       intf]

      [(meta/instance _ comp-name)
       ; We first need to check that comp-name refers to a component, in case it
       ; has not been checked already (for instance if the instance is declared later).
       (define comp (meta/instance-component target))
       ; Check that a port with that name exists in the component.
       (meta/design-unit-ref comp field-name
         (thunk (raise-semantic-error (format "Port not found in component ~a" (syntax-e comp-name)) parent-expr field-name)))
       ; Return the component.
       comp]

      [other other]))

  ; TODO allow to connect ports with multiplicity
  (define (check-composite-assignment stx target expr port)
    (define intf (lookup (meta/composite-port-intf-name port)))
    (define/syntax-parse (stmt ...)
      (for/list ([(name intf-port) (in-dict (meta/design-unit-fields intf))])
        (define/syntax-parse target^ (quasisyntax/loc stx (field-expr #,target #,name)))
        (define/syntax-parse expr^   (quasisyntax/loc stx (field-expr #,expr   #,name)))
        (match intf-port
          [(meta/data-port _ mode)
           (check
             (if (equal? mode (if (flip? #'target^) 'in 'out))
               (syntax/loc stx (assignment target^ expr^))
               (syntax/loc stx (assignment expr^   target^))))]
          [(meta/composite-port _ intf-name _ _)
           (check-composite-assignment stx #'target^ #'expr^ intf-port)])))
    (syntax/loc stx (begin stmt ...))))
