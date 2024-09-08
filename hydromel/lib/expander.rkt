; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  racket/stxparam
  racket/splicing
  data/pvector
  "signal.rkt"
  "slot.rkt"
  "std.rkt"
  "types.rkt"
  "function.rkt"
  "errors.rkt"
  (for-syntax
    racket/syntax
    racket/function
    racket/dict
    syntax/stx))

(provide
  design-unit interface component
  parameter data-port composite-port alias
  typedef constant local-signal instance
  if-statement for-statement statement-block
  assignment
  literal-expr name-expr field-expr
  indexed-port-expr
  call-expr call-expr/cast register-expr
  slot-expr signal-expr lift-expr
  choices
  array-for-expr concat-for-expr
  expression-type)

; ------------------------------------------------------------------------------
; Design units
; ------------------------------------------------------------------------------

(begin-for-syntax
  (define (stx-filter stx-lst id-lst)
    (for/list ([it (in-list stx-lst)]
               #:when (member (syntax-e (stx-car it)) id-lst))
      it))

  ; Return the list of ports, signals and instances in the given syntax object.
  (define (design-unit-fields stx-lst)
    (stx-filter stx-lst '(constant data-port composite-port local-signal alias instance if-statement for-statement)))

  ; Return the list of parameters in the given syntax object.
  (define (design-unit-parameters stx-lst)
    (stx-filter stx-lst '(parameter)))

  ; Return the list of port and signal names in the given syntax object.
  (define (design-unit-field-names stx-lst)
    (define/syntax-parse ((_ name _ ...) ...) (design-unit-fields stx-lst))
    (attribute name))

  ; Return the list of spliced composite ports in the given syntax object.
  (define (design-unit-spliced-names stx-lst)
    (filter identity
      (for/list ([stx (in-list stx-lst)])
        (syntax-parse stx
          #:literals [composite-port splice]
          [(composite-port name _ ... splice _ ...) #'name]
          [_                                        #f])))))

; A flag that allows to check whether a given form is part of a design unit body,
; or whether it appears at the module level.
(define-syntax-parameter in-design-unit #f)

; An interface or a component expands to a constructor function
; that returns a hash-map with public or debug data.
; Interfaces and components differ by the element types they are allowed
; to contain, but the expansion rule is exactly the same.
(define-syntax-parse-rule (design-unit name body ...)
  #:with (param        ...) (design-unit-parameters    (attribute body))
  #:with (arg-name     ...) (generate-temporaries      (attribute param))
  #:with (field-name   ...) (design-unit-field-names   (attribute body))
  #:with (spliced-name ...) (design-unit-spliced-names (attribute body))
  (begin
    (provide name)
    (define (name arg-name ...)
      (define-parameter-slot param arg-name) ...
      (splicing-syntax-parameterize ([in-design-unit #t])
        body ...)
      (define res (make-hash `((* . (spliced-name ...)) (field-name . ,field-name) ...)))
      (type-check res)
      res)))

(define-syntax-parse-rule (interface name body ...)
  (design-unit name body ...))

(define-syntax-parse-rule (component name body ...)
  (design-unit name body ...))

; ------------------------------------------------------------------------------
; Declarations
; ------------------------------------------------------------------------------

; Parameters are expanded in macro design-unit.
(define-syntax-parse-rule (parameter _ ...)
  (begin))

(define-syntax-parse-rule (define-parameter-slot param arg-name)
  #:with (_ param-name param-type) #'param
  (assert-type t param param-type
    (define param-name (make-slot #'param arg-name (const-type arg-name t) (thunk (make-const-type arg-name))))
    (type-check param-name)))

(define-syntax-parse-rule (typedef name param ... expr)
  #:with ((_ param-name param-type) ...) #'(param ...)
  (begin
    (assert-type t param param-type) ...
    (provide name)
    (define (name param-name ...) expr)
    (define-function/return-type name
      (λ (param-name ...)
        (assert-const 0 param-name ...)
        (assert-<:    0 (~@ param-name param-type) ...)
        ; TODO Is it relevant to return a const-type?
        (make-const-type (name (const-type-value param-name) ...))))))

; A constant infers its type immediately before computing its value.
; Here, we benefit from the fact that expression-type will return a
; const-type where the expression has already been evaluated.
(define-syntax-parse-rule (define-constant-slot cste)
  #:with (_ name expr) #'cste
  (begin
    (typing-functions expr)
    (define name (let ([t (expression-type expr)])
                   (make-slot #'cste (const-type-value t) t)))))

; A constant expands to a slot assigned to a variable.
(define-syntax-parser constant
  ; Local constant in an interface or component.
  [cste #:when (syntax-parameter-value #'in-design-unit)
   #'(define-constant-slot cste)]
  ; Module-level constant.
  [(_ name expr)
   #:with name^ (format-id #'name "~a$constant" #'name)
   #:with cste^ (quasisyntax/loc this-syntax (constant name^ expr))
   #'(begin
       (provide name name^)
       (define-constant-slot cste^)
       (define name (slot-data name^)))])

; An alias expands to a partial access to the target port.
; The alias and the corresponding port must refer to the same slot.
(define-syntax-parse-rule (alias name port-name)
  (define name (dict-ref port-name 'name)))

(define-syntax-parse-rule (assert-type target stx type-expr body ...)
  (begin
    (typing-functions type-expr)
    (splicing-let ([t (expression-type type-expr)])
      (parameterize ([current-typecheck-stx #'stx])
        (assert-const #f t)
        (assert-<:    #f t (type)))
      (splicing-let ([target (const-type-value t)])
        body ...))))

; A data port expands to a variable containing an empty slot.
; The slot is relevant for input ports because they are assigned from outside
; the current component instance.
; We use a slot for output ports as well to keep a simple port access mechanism.
(define-syntax-parse-rule (data-port name _ type-expr)
  #:with stx this-syntax
  (assert-type t stx type-expr
    (define name (make-slot #'stx #f t))))

; A local signal expands to a variable containing the result of the given
; expression in a slot.
; Like output ports, local signals do not need to be wrapped in a slot, but
; it helps expanding expressions without managing several special cases.
(define-syntax-parser local-signal
  [(_ name expr)
   #'(begin
       (typing-functions expr)
       (define name (make-slot #f expr #f (typer-for expr))))]
  [(~and (_ name type-expr expr) sig)
   #'(assert-type t sig type-expr
       (typing-functions expr)
       (define name (make-slot #'sig expr t (typer-for expr))))])

; A composite port expands to a variable that stores the result of a constructor
; call for the corresponding interface.
; If a multiplicity is present, a vector of channels is created.
(define-syntax-parse-rule (composite-port name (mult ...) (~or* (~literal splice) (~literal flip)) ... intf-name arg ...)
  (define name (channel (mult ...) intf-name arg ...)))

; An instance expands to a variable that stores the result of a constructor call
; for the corresponding component.
; If a multiplicity is present, a vector of channels is created.
(define-syntax-parse-rule (instance name (mult ...) comp-name arg ...)
  (define name (channel (mult ...) comp-name arg ...)))

(define-syntax-parser channel
  [(_ (m ms ...) intf-name arg ...)
   #'(build-vector m (λ (z) (channel (ms ...) intf-name arg ...)))]

  [(_ () intf-name arg ...)
   #'(intf-name arg ...)])

; ------------------------------------------------------------------------------
; Statements
; ------------------------------------------------------------------------------

; An assignment fills the target port's slot with the signal
; from the right-hand side.
(define-syntax-parse-rule (assignment target expr)
  #:with slt (syntax-parse #'target
               #:literals [name-expr]
               [(name-expr name) #'name]
               [_                #'target])
  (begin
    (typing-functions expr)
    (update-slot! slt expr (typer-for expr))))

; An if statement expands to a conditional statement that generates a hash map.
; That hash map is assigned to a variable with the same name as the if label.
(define-syntax-parse-rule (if-statement name (~seq condition then-body) ... else-body)
  (define name (cond [(int->bool condition) then-body]
                     ...
                     [else else-body])))

(define-syntax-parse-rule (for-statement name iter-name iter-expr body ...)
  #:with iter-name^ (generate-temporary #'iter-name)
  (define name (for/vector ([iter-name^ (in-list iter-expr)])
                 (define iter-name (make-slot #f iter-name^ (make-const-type iter-name^)))
                 body ...)))

; A statement block executes statements and returns a hash map that exposes
; local data for debugging.
(define-syntax-parse-rule (statement-block body ...)
  #:with (field-name ...) (design-unit-field-names (attribute body))
  (let ()
    body ...
    (make-hash `((field-name . ,field-name) ...))))

; ------------------------------------------------------------------------------
; Expressions
; ------------------------------------------------------------------------------

; A literal expression expands to its value.
(define-syntax-parser literal-expr
  [(_ value:integer)    #'value]
  [(_ value:identifier) #'(quote value)])

; A name expression expands to the corresponding variable name in the current scope.
; If the name refers to a slot, unwrap it.
; A name can also refer to a composite port...
(define-syntax-parse-rule (name-expr name ...)
  (slot-data* (concat-name name ...)))

; Append a suffix to a name-expr if specified.
(define-syntax-parser concat-name
  [(_ name)        #'name]
  [(_ name suffix) (format-id #'name "~a~a" #'name #'suffix)])

; After semantic checking, a field expression may contain the name
; of a record type where the field is declared.
(define-syntax-parser field-expr
  ; If a field expression contains a type name, generate a struct field accessor.
  [(_ expr field-name type-name)
   #:with acc (format-id #'field-name "~a-~a" #'type-name #'field-name)
   #'(acc expr)]
  ; If a field expression does not contain a type name, generate a dictionary access.
  [(_ expr field-name)
   #'(dict-ref expr 'field-name)])

; An indexed port expression expands to a vector access.
(define-syntax-parser indexed-port-expr
  [(_ expr)                   #'expr]
  [(_ expr index indices ...) #'(indexed-port-expr (vector-ref expr index) indices ...)])

; A call expression expands to a Racket function call.
(define-syntax-parse-rule (call-expr fn-name arg ...)
  (fn-name arg ...))

(define-syntax-parse-rule (call-expr/cast fn-name arg ...)
  #:with expr this-syntax
  ((expression-type expr) (fn-name arg ...)))

(define-syntax-parse-rule (choices expr ...)
  (list expr ...))

(define-syntax-parser register-expr
  #:literals [when-clause]
  [(_ i (when-clause r) d (when-clause e)) #'(register/re i r e d)]
  [(_ i (when-clause r) d)                 #'(register/r  i r   d)]
  [(_ i d (when-clause e))                 #'(register/e  i   e d)]
  [(_ i d)                                 #'(register    i     d)])

; A slot expression is a wrapper element added by the semantic checker to
; identify an expression that refers to a port or local signal for reading.
; It expands to a deferred signal read.
(define-syntax-parse-rule (slot-expr expr)
  (signal-defer (unwrap-slot expr)))

; A static signal expression is a wrapper element added by the semantic checker
; when an expression has a constant value, but is assigned to a signal.
(define-syntax-parse-rule (signal-expr expr)
  (make-signal (unwrap-slot expr)))

(define-syntax-parser unwrap-slot
  #:literals [name-expr field-expr]
  [(_ (~and (name-expr  _ ...) expr)) #'expr]
  [(_ (~and (field-expr _ ...) expr)) #'(slot-data expr)]
  [(_ expr)                           #'expr])

; A lift expression is a wrapper element added by the semantic checker
; when an expression depends on some signal values.
; name ... is a list of signal names that are needed to compute expr.
(define-syntax-parser lift-expr
  #:literals [slot-expr]
  ; Lift a signal expression. Since the expression returns a signal,
  ; we must lift signal-first to avoid creating a signal of signals.
  ; This is typically used when indexed-port-expr contains signals as indices.
  [(_ binding ...+ (slot-expr expr))
   #'(lift-expr binding ... (signal-first (slot-expr expr)))]
  ; Lift any expression that computes values from values.
  ; expr must not contain elements of type slot-expr.
  [(_ (name sexpr) ...+ expr)
   #'(for/signal ([name sexpr] ...) expr)])

(define-syntax-parser array-for-expr
  #:literals [lift-expr]
  [(_ (lift-expr binding ... body) (~seq iter-name iter-expr) ...)
   #'(signal-bundle-vector (for*/vector ([iter-name (in-list iter-expr)] ...)
                             (lift-expr binding ... body)))]

  [(_ body (~seq iter-name iter-expr) ...)
   #'(for*/pvector ([iter-name (in-list iter-expr)] ...)
       body)])

(define-syntax-parser concat-for-expr
  #:literals [lift-expr]
  ; When a lift-expr is wrapped in a comprehension, we evaluate it iteratively
  ; to generate a list of signals. Then we convert it to a signal of lists
  ; that we can concatenate.
  [(_ (lift-expr binding ... body) (~seq iter-name iter-expr) ...)
   #'(for/signal ([lst (signal-bundle-list (for*/list ([iter-name (in-list iter-expr)] ...)
                                             (lift-expr binding ... body)))])
       (for/fold ([res 0])
                 ([it (in-list lst)])
         (bitwise-ior (arithmetic-shift res 1) it)))]

  [(_ body (~seq iter-name iter-expr) ...)
   #'(for*/fold ([res 0])
                ([iter-name (in-list iter-expr)] ...)
       (bitwise-ior (arithmetic-shift res 1) body))])

; ------------------------------------------------------------------------------
; Type inference and checking
; ------------------------------------------------------------------------------

; Generate type inference code.
; Memoize the computed type if it is needed at runtime.
; This code is meant to be executed lazily, either in a slot type
; or inside a signal, so that out-of-order dependencies are correctly handled.
; Only constants infer their types immediately.
(define-syntax-parser expression-type
  #:literals [name-expr literal-expr slot-expr signal-expr]
  [(_ (name-expr name ...))
   #'(slot-type (concat-name name ...))]

  [(_ ((~or* slot-expr signal-expr) (~and ((~or* name-expr literal-expr) _ ...) body)))
   #'(expression-type body)]

  [(_ (~and (literal-expr _) this-expr))
   #'(make-const-type this-expr)]

  [(_ this-expr)
   #:with lbl (type-label #'this-expr)
   #'(lbl)])

(define-syntax-parser typer-for
  #:literals [name-expr literal-expr slot-expr signal-expr]
  [(_ (~and (~or* (name-expr _ ...)
                  (literal-expr _)
                  (slot-expr (name-expr _ ...))
                  (signal-expr (name-expr _ ...))
                  (signal-expr (literal-expr _ ...)))
            this-expr))
   #'(thunk (expression-type this-expr))]

  [(_ this-expr)
   (type-label #'this-expr)])

; Expression types are memoized in a dictionary whose keys are:
; either the 'label syntax property (generated in checker.rkt)
; or the source location of the expression (for expander tests only).
(define-for-syntax (type-label stx)
  (syntax-parse stx
    #:literals [slot-expr signal-expr]
    [((~or* slot-expr signal-expr) body)
     (type-label #'body)]

    [_
     (format-id stx "~a$type"
       (or
         (syntax-property stx 'label)
         (string->symbol (format "~a$~a:~a:~a"
                                 (syntax-source stx)
                                 (syntax-line stx)
                                 (syntax-column stx)
                                 (syntax-span stx)))))]))

(define-syntax-parse-rule (comprehension-slot left right)
  (make-slot #f #f (common-supertype (type-of left) (type-of right))))

(define-syntax-parser typing-functions
  #:literals [slot-expr signal-expr lift-expr array-for-expr concat-for-expr name-expr literal-expr]

  [(_ ((~or* slot-expr signal-expr) body))
   #'(typing-functions body)]

  [(_ (~and (lift-expr (name expr) ...+ body) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #'(begin
       (typing-functions expr) ...
       (splicing-let ([name (make-slot #f #f #f (typer-for expr))] ...)
         (typing-functions body)
         (define this-type-label (typer-for body))))]

  [(_ (~and (array-for-expr body (~seq iter-name iter-expr) ...) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #:with (~and (first-rng _ ...) (rng ...)) (generate-temporaries (attribute iter-name))
   #'(splicing-letrec-values ([(rng ...) (for*/lists (rng ...)
                                                     ([iter-name (in-list iter-expr)] ...)
                                           (values iter-name ...))]
                              [(len) (length first-rng)]
                              [(iter-name) (comprehension-slot (apply min rng) (apply max rng))] ...)
       (typing-functions body)
       (define (this-type-label)
         (array-type len (expression-type body))))]

  [(_ (~and (concat-for-expr body (~seq iter-name iter-expr) ...) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #:with (~and (first-rng _ ...) (rng ...)) (generate-temporaries (attribute iter-name))
   ; TODO Check that (expression-type body) is (abstract-integer 1) for all iterator values
   #'(splicing-letrec-values ([(rng ...) (for*/lists (rng ...)
                                                     ([iter-name (in-list iter-expr)] ...)
                                           (values iter-name ...))]
                              [(len) (length first-rng)]
                              [(iter-name) (comprehension-slot (apply min rng) (apply max rng))] ...)
       (typing-functions body)
       (define (this-type-label)
         (resize (expression-type body) len)))]

  [(_ ((~or* name-expr literal-expr) _ ...))
   #'(begin)]

  [(_ (~and (_ arg ...) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #'(begin
       (typing-functions arg) ...
       (define (this-type-label)
         (typing-function-body this-expr)))]

  [_ #'(begin)])

; Generate type inference code for an expression.
; TODO Warn on circular dependencies in register-expr
(define-syntax-parser typing-function-body
  #:literals [choices register-expr when-clause
              field-expr call-expr call-expr/cast expression-type]
  ; This is a special case for (expression-type) forms generated in checker.rkt
  ; Maybe we should generate these forms in expander instead.
  [(_ (~and (expression-type expr) this-expr))
   #'(make-const-type this-expr)]

  [(_ (choices expr ...))
   #'(tuple-type (list (expression-type expr) ...))]

  [(_ (register-expr i (~optional (when-clause r)) d (~optional (when-clause e))))
   #'(union-type (list (expression-type i) (expression-type d)))]

  [(_ (field-expr expr field-name))
   #'(slot-type (dict-ref (remove-dynamic-indices expr) 'field-name))]

  [(_ (~and ((~or* call-expr call-expr/cast) name arg ...) expr))
   #:with rt (function-return-type-name #'name)
   #'(rt #'expr (expression-type arg) ...)]

  [_ #'(any-type)])

; Replace dynamic indices with zeros in indexed expressions.
; This only concerns access to interface ports with multiplicity,
; which is expressed as a combination of field and indexed expressions.
(define-syntax-parser remove-dynamic-indices
  #:literals [indexed-port-expr field-expr]
  [(_ (indexed-port-expr expr index ...))
   #:with (index0 ...) (map (const #'(literal-expr 0)) (attribute index))
   #'(indexed-port-expr (remove-dynamic-indices expr) index0 ...)]

  [(_ (field-expr expr field-name))
   #'(field-expr (remove-dynamic-indices expr) field-name)]

  [(_ other) #'other])

(define (type-check inst)
  (match inst
    [(slot stx _ t _) #:when (and stx t)
     (define u (slot-type* inst))
     (unless (<: u t)
       (raise-type-error stx 0 (type->string u) (type->string t)))]

    [(hash-table ('* _) (_ vs) ...)
     (for-each type-check vs)]

    [(vector vs ...)
     (for-each type-check vs)]

    [_ (void)]))

; ------------------------------------------------------------------------------
; Disabled forms.
; ------------------------------------------------------------------------------

(define-syntax-parse-rule (disable-forms id ... msg)
  (begin
    (provide id) ...
    (define-syntax (id stx)
      (raise-syntax-error #f msg stx))
    ...))

; Multiplicity indications are processed in macros composite-port and instance.
(disable-forms splice flip
  "should be used inside a composite port declaration")

; When clauses are processed in macro register-expr.
(disable-forms when-clause
  "should be used inside a register expression")

; Concatenation expressions are converted to function calls in the checker.
(disable-forms import or-expr and-expr rel-expr add-expr mult-expr shift-expr
               if-expr case-expr prefix-expr range-expr slice-expr concat-expr
               indexed-array-expr array-expr cast-expr assign-expr record-type-expr
               array-assoc-expr array-assoc slice-assoc-expr slice-assoc record-expr
               tuple-expr
  "should not be used outside of begin-hydromel")
