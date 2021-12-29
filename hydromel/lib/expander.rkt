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
  (for-syntax
    racket/match
    racket/syntax
    racket/function
    racket/dict
    syntax/stx))

(provide
  design-unit interface component
  parameter data-port composite-port alias
  typedef constant local-signal instance
  if-statement for-statement statement-block
  assignment connect-statement
  literal-expr name-expr field-expr
  indexed-port-expr
  call-expr call-expr/cast register-expr
  slot-expr signal-expr lift-expr
  choices
  array-for-expr concat-for-expr
  type-of)

; ------------------------------------------------------------------------------
; Design units
; ------------------------------------------------------------------------------

(begin-for-syntax
  (define (stx-filter stx-lst id-lst)
    (for/list ([it (in-list stx-lst)]
               #:when (member (syntax-e (stx-car it)) id-lst))
      it))

  ; Make a constructor name for a design unit.
  (define (design-unit-ctor-name name)
    (format-id name "~a-make" name))

  ; Return the list of ports, signals and instances in the given syntax object.
  (define (design-unit-fields stx-lst)
    (stx-filter stx-lst '(constant data-port composite-port local-signal alias instance if-statement for-statement)))

  ; Return the list of parameter names in the given syntax object.
  (define (design-unit-parameter-names stx-lst)
    (define/syntax-parse ((parameter name _) ...) (stx-filter stx-lst '(parameter)))
    (attribute name))

  (define (design-unit-parameter-types stx-lst)
    (define/syntax-parse ((parameter _ type) ...) (stx-filter stx-lst '(parameter)))
    (attribute type))

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
  #:with ctor-name          (design-unit-ctor-name #'name)
  #:with (param-name ...)   (design-unit-parameter-names (attribute body))
  #:with (param-type ...)   (design-unit-parameter-types (attribute body))
  #:with (arg-name   ...)   (generate-temporaries        (attribute param-name))
  #:with (field-name ...)   (design-unit-field-names     (attribute body))
  #:with (spliced-name ...) (design-unit-spliced-names   (attribute body))
  (begin
    (provide ctor-name)
    (define (ctor-name arg-name ...)
      (define param-name (make-slot arg-name (static-data arg-name param-type) (static-data arg-name (literal-type arg-name)))) ...
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

; Create a slot for given data and type.
; This is implemented as a macro because we want to evaluate
; expression types lazily.
(define-syntax-parser make-slot
  #:literals [type-of]
  [(_ data type (type-of expr))
   #'(let ([t type])
       (slot data t (make-slot-actual-typer t expr)))]

  [(_ data (type-of expr))
   #'(make-slot data #f (type-of expr))]

  [(_ (type-of expr))
   #'(make-slot #f #f (type-of expr))]

  [(_ data type expr-type)
   #'(slot data type (λ (a) (if a expr-type type)))]

  [(_ data type)
   #'(make-slot data type type)]

  [(_ type)
   #'(make-slot #f type type)])

(define-syntax-parse-rule (make-slot-actual-typer default-type expr)
  #:with typer (type-label #'expr)
  (make-slot-actual-typer* default-type typer))

(define (make-slot-actual-typer* default-type actual-typer)
  (let ([res      #f]
        [visiting #f])
    (λ (actual)
      (cond [(and default-type (not actual)) default-type]
            [res res]
            ; TODO display signal names, locate error in source code
            [visiting (or default-type (error "Could not infer type due to cross-dependencies"))]
            [else (set! visiting #t)
                  (set! res (actual-typer))
                  (set! visiting #f)
                  res]))))

(define-syntax-parse-rule (update-slot! slt expr)
  (let ([s slt])
    (set-slot-data!         s expr)
    (set-slot-actual-typer! s (make-slot-actual-typer (slot-declared-type s) expr))))

; Parameters are expanded in macro design-unit.
(define-syntax-parse-rule (parameter _ ...)
  (begin))

(define-syntax-parse-rule (typedef name ((~literal parameter) param-name param-type) ... expr)
  #:with (arg-name   ...) (generate-temporaries (attribute param-name))
  #:with impl-name  (format-id #'name "~a:impl"             #'name)
  #:with rtype-name (format-id #'name "~a:impl:return-type" #'name)
  (begin
    (provide impl-name rtype-name)
    (define (impl-name arg-name ...)
      (define param-name (make-slot arg-name (static-data arg-name param-type) (static-data arg-name (literal-type arg-name)))) ...
      (type-check param-name) ...
      expr)
    (define (rtype-name arg-name ...)
      (static-data (impl-name (static-data-value arg-name) ...) (type:impl)))))


; A constant expands to a slot assigned to a variable.
(define-syntax-parser constant
  ; Local constant in an interface or component.
  [(constant name expr) #:when (syntax-parameter-value #'in-design-unit)
   #'(begin
       (typing-functions expr)
       (define name (constant->slot expr)))]

  ; Module-level context constant.
  [(constant name expr)
   #:with name^ (format-id #'name "~a-constant" #'name)
   #'(begin
       (provide name^)
       (typing-functions expr)
       (define name^ (constant->slot expr)))])

; A constant infers its type immediately before computing its value.
; Here, we benefit from the fact that type-of will return a
; static-data where the expression has already been evaluated.
(define-syntax-parse-rule (constant->slot expr)
  (let ([expr-type (type-of expr)])
    (make-slot (static-data-value expr-type) expr-type)))

; An alias expands to a partial access to the target port.
; The alias and the corresponding port must refer to the same slot.
(define-syntax-parse-rule (alias name port-name)
  (define name (dict-ref port-name 'name)))

; A data port expands to a variable containing an empty slot.
; The slot is relevant for input ports because they are assigned from outside
; the current component instance.
; We use a slot for output ports as well to keep a simple port access mechanism.
(define-syntax-parse-rule (data-port name _ type)
  (define name (make-slot type)))

; A local signal expands to a variable containing the result of the given
; expression in a slot.
; Like output ports, local signals do not need to be wrapped in a slot, but
; it helps expanding expressions without managing several special cases.
(define-syntax-parser local-signal
  [(_ name expr)
   #'(begin
       (typing-functions expr)
       (define name (make-slot expr (type-of expr))))]

  [(_ name type expr)
   #'(begin
       (typing-functions expr)
       (define name (make-slot expr type (type-of expr))))])

; A composite port expands to a variable that stores the result of a constructor
; call for the corresponding interface.
; If a multiplicity is present, a vector of channels is created.
(define-syntax-parse-rule (composite-port name (mult ...) (~or (~literal splice) (~literal flip)) ... intf-name arg ...)
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
   #:with ctor-name (design-unit-ctor-name #'intf-name)
   #'(ctor-name arg ...)])

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
    (update-slot! slt expr)))

; Connect two interface instances.
; See std.rkt for the implementation of connect.
(define-syntax-parse-rule (connect-statement left right)
  (connect left right))

; An if statement expands to a conditional statement that generates a hash map.
; That hash map is assigned to a variable with the same name as the if label.
(define-syntax-parse-rule (if-statement name (~seq condition then-body) ... else-body)
  (define name (cond [(int->bool:impl condition) then-body]
                     ...
                     [else else-body])))

(define-syntax-parse-rule (for-statement name iter-name iter-expr body ...)
  #:with iter-name^ (generate-temporary #'iter-name)
  (define name (for/vector ([iter-name^ (in-list iter-expr)])
                 (define iter-name (make-slot iter-name^ (static-data iter-name^ (literal-type iter-name^))))
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
  [(field-expr expr field-name type-name)
   #:with acc (format-id #'field-name "~a-~a" #'type-name #'field-name)
   #'(acc expr)]
  ; If a field expression does not contain a type name, generate a dictionary access.
  [(field-expr expr field-name)
   #'(dict-ref expr 'field-name)])

; An indexed port expression expands to a vector access.
(define-syntax-parse-rule (indexed-port-expr expr index)
  (vector-ref expr index))

; A call expression expands to a Racket function call.
(define-syntax-parse-rule (call-expr fn-name arg ...)
  (fn-name arg ...))

(define-syntax-parse-rule (call-expr/cast fn-name arg ...)
  #:with expr this-syntax
  ((type-of expr) (fn-name arg ...)))

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
  (signal (unwrap-slot expr)))

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
(define-syntax-parse-rule (type-of expr)
   #:with key (type-label #'expr)
   (key))

; Expression types are memoized in a dictionary whose keys are:
; either the 'label syntax property (generated in checker.rkt)
; or the source location of the expression (for expander tests only).
(define-for-syntax (type-label stx)
  (format-id stx "type-of:~a"
    (or
      (syntax-property stx 'label)
      (string->symbol (format "~a$~a:~a:~a"
                              (syntax-source stx)
                              (syntax-line stx)
                              (syntax-column stx)
                              (syntax-span stx))))))

(define-syntax-parser typing-functions
  #:literals [slot-expr signal-expr lift-expr array-for-expr concat-for-expr]

  [(_ (~and ((~or* slot-expr signal-expr) body) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #:with body-type-label (type-label #'body)
   #'(begin
       (typing-functions body)
       (define this-type-label body-type-label))]

  [(_ (~and (lift-expr (name val) ...+ body) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #:with body-type-label (type-label #'body)
   #'(begin
       (typing-functions val) ...
       (splicing-let ([name (make-slot (type-of val))] ...)
         (typing-functions body)
         (define this-type-label body-type-label)))]

  [(_ (~and (array-for-expr body (~seq iter-name iter-expr) ...) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #:with (rng ...)   (generate-temporaries (attribute iter-name))
   #:with (left ...)  (generate-temporaries (attribute iter-name))
   #:with (right ...) (generate-temporaries (attribute iter-name))
   #'(splicing-letrec ([rng       iter-expr] ...
                       [left      (first rng)] ...
                       [right     (last rng)] ...
                       [iter-name (make-slot (common-supertype (literal-type left) (literal-type right)))] ...)
       (typing-functions body)
       (define (this-type-label)
         (array (* (add1 (abs (- left right))) ...) (type-of body))))]

  [(_ (~and (concat-for-expr body (~seq iter-name iter-expr) ...) this-expr))
   #:with this-type-label (type-label #'this-expr)
   #:with (rng ...)   (generate-temporaries (attribute iter-name))
   #:with (left ...)  (generate-temporaries (attribute iter-name))
   #:with (right ...) (generate-temporaries (attribute iter-name))
   ; TODO Check that (type-of body) is (abstract-integer 1) for all iterator values
   #'(splicing-letrec ([rng       iter-expr] ...
                       [left      (first rng)] ...
                       [right     (last rng)] ...
                       [iter-name (make-slot (common-supertype (literal-type left) (literal-type right)))] ...)
       (typing-functions body)
       (define (this-type-label)
         (resize (type-of body) (* (add1 (abs (- left right))) ...))))]

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
  #:literals [name-expr literal-expr choices register-expr when-clause
              field-expr call-expr call-expr/cast type-of]
  ; This is a special case for (type-of) forms generated in checker.rkt
  ; Maybe we should generate these forms in expander instead.
  [(_ (~and (type-of expr) this-expr))
   #'(static-data this-expr (call-expr type:impl))]

  [(_ (name-expr name ...))
   #'(slot-type (concat-name name ...))]

  [(_ (~and (literal-expr _) this-expr))
   #'(static-data this-expr (literal-type this-expr))]

  [(_ (choices expr ...))
   #'(tuple (list (type-of expr) ...))]

  [(_ (register-expr i (~optional (when-clause r)) d (~optional (when-clause e))))
   #'(union (list (type-of i) (type-of d)))]

  [(_ (field-expr expr field-name))
   #'(slot-type (dict-ref (remove-dynamic-indices expr) 'field-name))]

  [(_ (~and ((~or* call-expr call-expr/cast) name arg ...) expr))
   #:with rt (format-id #'name "~a:return-type" #'name)
   #:with (tv ...) (generate-temporaries (attribute arg))
   #'(begin
       (define tv (type-of arg)) ...
       (define res-type (rt tv ...))
       (if (and (static-data? tv) ...)
         (static-data (name arg ...) res-type)
         res-type))]

  [_ #'(any)])

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
    [(slot _ t _) #:when t
     (define u (slot-type* inst))
     (unless (<: u t)
       ; TODO show source code instead of generated code, or source location only.
       (raise-result-error 'type-check (format-type t) (format-type u)))]

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
               indexed-array-expr array-expr cast-expr assign-expr record-type
               record-expr
  "should not be used outside of begin-tiny-hdl")
