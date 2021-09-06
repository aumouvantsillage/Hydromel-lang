#lang racket

(require
  syntax/parse/define
  racket/stxparam
  racket/splicing
  "signal.rkt"
  "slot.rkt"
  "std.rkt"
  "types.rkt"
  (for-syntax
    racket/match
    racket/syntax
    syntax/stx))

(provide
  import
  interface
  component
  parameter
  data-port
  composite-port
  multiplicity
  flip
  splice
  constant
  local-signal
  instance
  if-statement
  for-statement
  statement-block
  assignment
  connect-statement
  literal-expr
  alias
  name-expr
  field-expr
  indexed-expr
  call-expr
  register-expr
  when-clause
  slot-expr
  signal-expr
  lift-expr concat-expr
  or-expr and-expr rel-expr add-expr mult-expr if-expr prefix-expr range-expr slice-expr
  infer-type)

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
    (attribute name)))

; A flag that allows to check whether a given form is part of a design unit body,
; or whether it appears at the module level.
(define-syntax-parameter in-design-unit #f)

; A hash table of inferred types for expressions in a given context.
(define-syntax-parameter inferred-types
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside design-unit")))

; A list of type checking thunks to run after all slots have been filled.
(define-syntax-parameter type-checks
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside design-unit")))

; An interface or a component expands to a constructor function
; that returns a hash-map with public or debug data.
; Interfaces and components differ by the element types they are allowed
; to contain, but the expansion rule is exactly the same.
(define-syntax-parse-rule (design-unit name body ...)
  #:with ctor-name        (design-unit-ctor-name #'name)
  #:with (param-name ...) (design-unit-parameter-names (attribute body))
  #:with (param-type ...) (design-unit-parameter-types (attribute body))
  #:with (arg-name   ...) (generate-temporaries        (attribute param-name))
  #:with (field-name ...) (design-unit-field-names     (attribute body))
  (begin
    (provide ctor-name)
    (define (ctor-name arg-name ...)
      (define types  (make-hash))
      (define checks '())
      (splicing-syntax-parameterize ([in-design-unit #t]
                                     [inferred-types (make-rename-transformer #'types)]
                                     [type-checks    (make-rename-transformer #'checks)])
        (define param-name (make-slot arg-name (static-data arg-name param-type))) ...
        body ...)
      (for ([f (in-list (reverse checks))])
        (f))
      (make-hash `((field-name . ,field-name) ...)))))

(define-syntax-parse-rule (interface name body ...)
  (design-unit name body ...))

(define-syntax-parse-rule (component name body ...)
  (design-unit name body ...))

; ------------------------------------------------------------------------------
; Declarations
; ------------------------------------------------------------------------------

; Parameters are expanded in macro design-unit.
; TODO add type checking
(define-syntax-parse-rule (parameter _ ...)
  (begin))

; A constant expands to a slot assigned to a variable.
(define-syntax-parser constant
  ; Local constant in an interface or component.
  [(constant name expr) #:when (syntax-parameter-value #'in-design-unit)
   #'(define name (constant-to-slot expr))]

  ; Module-level context constant.
  [(constant name expr)
   #:with name^ (format-id #'name "~a-constant" #'name)
   #'(begin
       (provide name^)
       ; The expression will not be type-checked, so we create a temporary type
       ; dictionary that will be used only once.
       (define name^ (let ([types (make-hash)])
                       (syntax-parameterize ([inferred-types (make-rename-transformer #'types)])
                         (constant-to-slot expr)))))])

; A constant infers its type immediately before computing its value.
; Here, we benefit from the fact that infer-type will return a
; static-data where the expression has already been evaluated.
(define-syntax-parse-rule (constant-to-slot expr)
  (let ([t (infer-type expr)])
    (make-slot (static-data-value t) t)))

; An alias expands to a partial access to the target port.
; The alias and the corresponding port must refer to the same slot.
(define-syntax-parse-rule (alias name port-name)
  (define name (dict-ref port-name 'name)))

; A data port expands to a variable containing an empty slot.
; The slot is relevant for input ports because they are assigned from outside
; the current component instance.
; We use a slot for output ports as well to keep a simple port access mechanism.
(define-syntax-parse-rule (data-port name _ type)
  (define name (make-slot #f type)))

; A local signal expands to a variable containing the result of the given
; expression in a slot.
; Like output ports, local signals do not need to be wrapped in a slot, but
; it helps expanding expressions without managing several special cases.
(define-syntax-parse-rule (local-signal name (~optional type) expr)
  #:with expr-type   #'(infer-type expr)
  #:with target-type (or (attribute type) #'expr-type)
  (begin
    (define name (make-slot expr target-type))
    (add-type-check
      (unless (subtype? expr-type target-type)
        ; TODO show source code instead of generated code, or source location only.
        (raise-syntax-error #f (format "Expression type is incompatible with type annotation, found ~v, expected ~v" expr-type target-type) #'expr)))))

; A composite port expands to a variable that stores the result of a constructor
; call for the corresponding interface.
; If the multiplicity is greater than 1, a vector of channels is created.
(define-syntax-parse-rule (composite-port name (~optional (multiplicity mult)) (~or (~literal splice) (~literal flip)) ... intf-name arg ...)
  #:with ctor-name (design-unit-ctor-name #'intf-name)
  #:with m (or (attribute mult) #'1)
  (define name (let ([ctor (λ (z) (ctor-name arg ...))])
                 (if (> m 1)
                   (build-vector m ctor)
                   (ctor #f)))))

; An instance expands to a variable that stores the result of a constructor call
; for the corresponding component.
; If the multiplicity is greater than 1, a vector of channels is created.
; TODO We should create a vector if multiplicity is present regardless of its value.
(define-syntax-parse-rule (instance name (~optional ((~literal multiplicity) mult)) comp-name arg ...)
  #:with ctor-name (design-unit-ctor-name #'comp-name)
  #:with m (or (attribute mult) #'1)
  (define name (let ([ctor (λ (z) (ctor-name arg ...))])
                 (if (> m 1)
                   (build-vector m ctor)
                   (ctor #f)))))

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
  #:with target-type #'(infer-type target)
  #:with expr-type   #'(infer-type expr)
  (begin
    (set-slot-data! slt expr)
    (add-type-check
      (unless (subtype? expr-type target-type)
        ; TODO show source code instead of generated code, or source location only.
        (raise-syntax-error #f (format "Expression type is incompatible with target, found ~v, expected ~v" expr-type target-type) #'expr)))))

; Connect two interface instances.
; See std.rkt for the implementation of connect.
(define-syntax-parse-rule (connect-statement left right)
  (connect left right))

; An if statement expands to a conditional statement that generates a hash map.
; That hash map is assigned to a variable with the same name as the if label.
(define-syntax-parse-rule (if-statement name (~seq condition then-body) ... else-body)
  (define name (cond [(int-to-bool-impl condition) then-body]
                     ...
                     [else else-body])))

(define-syntax-parse-rule (for-statement name iter-name expr body ...)
  #:with iter-name^ (generate-temporary #'iter-name)
  (define name (for/vector ([iter-name^ (in-list expr)])
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
(define-syntax-parse-rule (literal-expr value)
  value)

; A name expression expands to the corresponding variable name in the current scope.
; If the name refers to a slot, unwrap it.
; A name can also refer to a composite port...
(define-syntax-parse-rule (name-expr name ...)
  #:with expr this-syntax
  #:with name^ #'(concat-name name ...)
  (if (slot? name^)
    (slot-data name^)
    (begin
      ; (printf "NOT A SLOT ~a ~a\n" '(name ...) name^)
      name^)))

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

; An indexed expression expands to a chain of vector accesses.
(define-syntax-parser indexed-expr
  [(indexed-expr expr index ... last)
   #'(vector-ref (indexed-expr expr index ...) last)]
  [(indexed-expr expr)
   #'expr])

; A call expression expands to a Racket function call.
(define-syntax-parse-rule (call-expr fn-name arg ...)
  #:with expr this-syntax
  ; The type acts as a function that casts its argument.
  ; If no type was found, it will be the identity function.
  (let ([type (infer-type expr)])
    (type (fn-name arg ...))))

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
  ; we must lift signal-first to avoid created a signal of signals.
  ; This is typically used when indexed-expr contains signals as indices.
  [(_ binding ...+ (slot-expr expr))
   #'(lift-expr binding ... (signal-first (slot-expr expr)))]
  ; Lift any expression that computes values from values.
  ; expr must not contain elements of type slot-expr.
  [(_ (name sexpr) ...+ expr)
   #'(for/signal ([name sexpr] ...) expr)])

; ------------------------------------------------------------------------------
; Type inference and checking
; ------------------------------------------------------------------------------

; Generate type inference code and memoize the computed type.
; This code is meant to be deferred, either in a slot type
; or inside a signal, so that out-of-order dependencies are correctly handled.
; Only constants infer their types immediately.
(define-syntax-parser infer-type
  #:literals [infer-type]
  ; This is a special case for (infer-type) forms generated in checker.rkt
  ; Maybe we can generate these forms in expander instead.
  [(_ (infer-type expr))
   #'(type (infer-type expr))]

  [(_ expr)
   #:with key (datum->syntax #'expr (format "~a$~a$~a"
                                     (syntax-source   #'expr)
                                     (syntax-position #'expr)
                                     (syntax-span     #'expr)))
   #'(dict-ref inferred-types key
       (thunk
         (let ([t (infer-type* expr)])
           (dict-set! inferred-types key t)
           t)))])

; Generate type inference code for an expression.
; TODO Warn on circular dependencies in register-expr
(define-syntax-parser infer-type*
  #:literals [name-expr literal-expr register-expr when-clause field-expr call-expr slot-expr signal-expr lift-expr]
  [(_ (name-expr name ...))
   #'(slot-type (concat-name name ...))]

  [(_ (literal-expr n))
   #'(static-data n (literal-type n))]

  [(_ (register-expr i (~optional (when-clause r)) d (~optional (when-clause e))))
   #:with infer-r (if (attribute r) #'(infer-type r) #'(void))
   #:with infer-e (if (attribute e) #'(infer-type e) #'(void))
   #'(begin
       infer-r
       infer-e
       (union (list (infer-type i) (infer-type d))))]

  [(_ (field-expr expr field-name))
   #'(let ([x (dict-ref (remove-dynamic-indices expr) 'field-name)])
       (slot-type x))]

  [(_ (~and (call-expr name arg ...) expr))
   #:with sig-name (format-id #'here "~a-signature" #'name)
   #'(let* ([arg-types (list (infer-type arg) ...)]
            [res-type (apply sig-name arg-types)])
       (if (andmap static-data? arg-types)
         (static-data (name arg ...) res-type)
         res-type))]

  ; TODO: do not implemented these since they will be implemented as call-exprs
  ; [(field-expr expr field-name type-name)]
  ; [(indexed-expr)]

  [(_ (signal-expr x))
   #'(infer-type x)]

  [(_ (slot-expr x))
   #'(infer-type x)]

  [(_ (lift-expr (name sexpr) ...+ expr))
   #'(let ([name (make-slot #f (infer-type sexpr))] ...)
       (infer-type expr))]

  [_ #'(any)])

; Replace dynamic indices with zeros in indexed expressions.
; This only concerns access to interface ports with multiplicity > 1,
; which is expressed as a combination of field and indexed expressions.
(define-syntax-parser remove-dynamic-indices
  #:literals [indexed-expr field-expr]
  [(_ (indexed-expr expr index ...))
   #:with (index0 ...) (for/list ([it (in-list (attribute index))]) #'0)
   #'(indexed-expr (remove-dynamic-indices expr) index0 ...)]

  [(_ (field-expr expr field-name))
   #'(field-expr (remove-dynamic-indices expr) field-name)]

  [(_ other) #'other])

(define-syntax-parse-rule (add-type-check expr)
  (set! type-checks (cons (thunk expr) type-checks)))

; ------------------------------------------------------------------------------
; Disabled forms.
; ------------------------------------------------------------------------------

(define-syntax-parse-rule (disable-forms id ... msg)
  (begin
    (define-syntax (id stx)
      (raise-syntax-error #f msg stx))
    ...))

; Multiplicity indications are processed in macros composite-port and instance.
(disable-forms multiplicity splice flip
  "should be used inside a composite port declaration")

; When clauses are processed in macro register-expr.
(disable-forms when-clause
  "should be used inside a register expression")

; Concatenation expressions are converted to function calls in the checker.
(disable-forms import or-expr and-expr rel-expr add-expr mult-expr
  if-expr prefix-expr range-expr slice-expr concat-expr
  "should not be used outside of begin-tiny-hdl")


(module+ test
  (require
    rackunit
    "helpers.rkt"
    "signal.rkt"
    "types.rkt")

  (define (check-sig-equal? t e n)
    (check-equal? (signal-take t n) (signal-take e n)))


  (interface I0
    (parameter N (call-expr unsigned (literal-expr 32)))
    (data-port x in  (call-expr signed (name-expr N)))
    (data-port y out (call-expr signed (name-expr N))))

  (define i0-inst (I0-make 30))

  (test-case "Can construct a channel for an interface with simple ports"
    (check-pred slot? (dict-ref i0-inst 'x))
    (check-pred slot? (dict-ref i0-inst 'y)))

  (component C0
    (parameter N (call-expr unsigned (literal-expr 32)))
    (data-port x in  (call-expr signed (name-expr N)))
    (data-port y out (call-expr signed (name-expr N)))
    (assignment (name-expr y) (slot-expr (name-expr x))))

  (define c0-inst (C0-make 30))
  (slot-set! (c0-inst x) (signal 23))

  (test-case "Can construct a channel for a component with simple ports"
    (check-pred slot? (dict-ref c0-inst 'x))
    (check-pred slot? (dict-ref c0-inst 'y)))

  (test-case "Can assign a simple port to another simple port"
    (check-sig-equal? (slot-ref c0-inst y) (slot-ref c0-inst x) 5))

  (test-case "Can infer the type of a port"
    (check-equal? (slot-type (dict-ref c0-inst 'x)) (signed 30)))

  (interface I1
    (data-port x in  (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32))))

  (interface I2
    (data-port z in (call-expr signed (literal-expr 32)))
    (composite-port i I1)
    (data-port u out (call-expr signed (literal-expr 32))))

  (interface I3
    (data-port v in (call-expr signed (literal-expr 32)))
    (composite-port j I2)
    (data-port w out (call-expr signed (literal-expr 32))))

  (define i2-inst (I2-make))
  (define i3-inst (I3-make))

  (test-case "Can construct a channel for an interface with composite ports and no parameters"
    (check-pred slot? (dict-ref i2-inst 'z))
    (check-pred dict? (dict-ref i2-inst 'i))
    (check-pred slot? (dict-ref (dict-ref i2-inst 'i) 'x))
    (check-pred slot? (dict-ref (dict-ref i2-inst 'i) 'y))
    (check-pred slot? (dict-ref i2-inst 'u))

    (check-pred slot? (dict-ref i3-inst 'v))
    (check-pred dict? (dict-ref i3-inst 'j))
    (check-pred slot? (dict-ref (dict-ref i3-inst 'j) 'z))
    (check-pred slot? (dict-ref (dict-ref (dict-ref i3-inst 'j) 'i) 'x))
    (check-pred slot? (dict-ref (dict-ref (dict-ref i3-inst 'j) 'i) 'y))
    (check-pred slot? (dict-ref (dict-ref i3-inst 'j) 'u))
    (check-pred slot? (dict-ref i3-inst 'w)))

  (component C1
    (data-port v in (call-expr signed (literal-expr 32)))
    (composite-port j I2)
    (data-port w out (call-expr signed (literal-expr 32)))
    (assignment (name-expr w) (slot-expr (name-expr v))))

  (define c1-inst (C1-make))

  (test-case "Can construct a channel for a component with composite ports and no parameters"
    (check-pred slot? (dict-ref c1-inst 'v))
    (check-pred dict? (dict-ref c1-inst 'j))
    (check-pred slot? (dict-ref (dict-ref c1-inst 'j) 'z))
    (check-pred slot? (dict-ref (dict-ref (dict-ref c1-inst 'j) 'i) 'x))
    (check-pred slot? (dict-ref (dict-ref (dict-ref c1-inst 'j) 'i) 'y))
    (check-pred slot? (dict-ref (dict-ref c1-inst 'j) 'u))
    (check-pred slot? (dict-ref c1-inst 'w)))

  (interface I4
    (composite-port i (multiplicity (literal-expr 3)) I1))

  (define i4-inst (I4-make))

  (test-case "Can construct a channel for an interface with a vector port"
    (check-pred vector? (dict-ref i4-inst 'i))
    (check-eq? (vector-length (dict-ref i4-inst 'i)) 3)
    (for ([n (kw-range-impl 0 2)])
      (check-pred dict? (vector-ref (dict-ref i4-inst 'i) n))))

  (interface I5
    (parameter N (call-expr unsigned (literal-expr 32)))
    (composite-port i (multiplicity (name-expr N)) I1))

  (define i5-inst (I5-make 5))

  (test-case "Can construct a channel for an interface with arguments"
    (check-pred vector? (dict-ref i5-inst 'i))
    (check-eq? (vector-length (dict-ref i5-inst 'i)) 5)
    (for ([n (kw-range-impl 0 4)])
      (check-pred dict? (vector-ref (dict-ref i5-inst 'i) n))))

  (interface I6
    (parameter M (call-expr unsigned (literal-expr 32)))
    (composite-port j I5 (name-expr M)))

  (define i6-inst (I6-make 3))

  (test-case "Can construct a channel containing a composite port with arguments"
    (check-pred vector? (dict-ref (dict-ref i6-inst 'j) 'i))
    (check-eq? (vector-length (dict-ref (dict-ref i6-inst 'j) 'i)) 3)
    (for ([n (kw-range-impl 0 2)])
      (check-pred dict? (vector-ref (dict-ref (dict-ref i6-inst 'j) 'i) n))))

  (component C2
    (composite-port i I1)
    (assignment (field-expr (name-expr i) y)
                (slot-expr (field-expr (name-expr i) x))))

  (define c2-inst (C2-make))
  (slot-set! (c2-inst i x) (signal 23))

  (test-case "Can access simple ports in a composite port"
    (check-sig-equal? (slot-ref c2-inst i y) (slot-ref c2-inst i x) 5))

  (component C3
    (composite-port i (multiplicity (literal-expr 3)) I1)
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y)
                (slot-expr (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y)
                (slot-expr (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 2)) y)
                (slot-expr (field-expr (indexed-expr (name-expr i) (literal-expr 2)) x))))

  (define c3-inst (C3-make))
  (slot-set! (c3-inst i 0 x) (signal 10))
  (slot-set! (c3-inst i 1 x) (signal 20))
  (slot-set! (c3-inst i 2 x) (signal 30))

  (test-case "Can access simple ports in a vector composite port with static indices"
    (check-sig-equal? (slot-ref c3-inst i 0 y) (slot-ref c3-inst i 0 x) 5)
    (check-sig-equal? (slot-ref c3-inst i 1 y) (slot-ref c3-inst i 1 x) 5)
    (check-sig-equal? (slot-ref c3-inst i 2 y) (slot-ref c3-inst i 2 x) 5))

  (interface I7
    (data-port x in (call-expr signed (literal-expr 32))))

  (component C4
    (composite-port i (multiplicity (literal-expr 3)) I7)
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z)
                (lift-expr [y^ (slot-expr (name-expr y))]
                           (slot-expr (field-expr (indexed-expr (name-expr i) (name-expr y^)) x)))))

  (define c4-inst (C4-make))
  (slot-set! (c4-inst i 0 x) (signal 10))
  (slot-set! (c4-inst i 1 x) (signal 20))
  (slot-set! (c4-inst i 2 x) (signal 30))
  (slot-set! (c4-inst y)     (signal 0 1 2 1 0 2))

  (test-case "Can access simple ports in a vector composite port with dynamic indices"
    (check-sig-equal? (slot-ref c4-inst z) (signal 10 20 30 20 10 30) 6))

  (component C5
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z)
      (lift-expr [x^ (slot-expr (name-expr x))]
                 [y^ (slot-expr (name-expr y))]
        (call-expr cast-impl
          (call-expr signed (literal-expr 32))
          (call-expr + (name-expr x^) (name-expr y^))))))

  (define c5-inst (C5-make))
  (slot-set! (c5-inst x) (signal 1  2  3  4  5))
  (slot-set! (c5-inst y) (signal 10 20 30 40 50))

  (test-case "Can perform an operation between signals"
    (check-sig-equal? (slot-ref c5-inst z) (signal 11 22 33 44 55) 5))

  (component C6
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u in (call-expr signed (literal-expr 32)))
    (data-port v out (call-expr signed (literal-expr 32)))
    (local-signal xy (lift-expr [x^ (slot-expr (name-expr x))]
                                [y^ (slot-expr (name-expr y))]
                                (call-expr * (name-expr x^) (name-expr y^))))
    (local-signal zu (lift-expr [z^ (slot-expr (name-expr z))]
                                [u^ (slot-expr (name-expr u))]
                                (call-expr * (name-expr z^) (name-expr u^))))
    (assignment (name-expr v)
      (lift-expr [xy^ (slot-expr (name-expr xy))]
                 [zu^ (slot-expr (name-expr zu))]
        (call-expr cast-impl
          (call-expr signed (literal-expr 32))
          (call-expr + (name-expr xy^) (name-expr zu^))))))

  (define c6-inst (C6-make))
  (slot-set! (c6-inst x) (signal 10 20 30 40 50))
  (slot-set! (c6-inst y) (signal 2))
  (slot-set! (c6-inst z) (signal 1 2 3 4 5))
  (slot-set! (c6-inst u) (signal 3))

  (test-case "Can use local signals"
    (check-sig-equal? (slot-ref c6-inst v) (signal 23 46 69 92 115) 5))

  (component C7
    (parameter N (call-expr unsigned (literal-expr 32)))
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y)
                (lift-expr [x^ (slot-expr (name-expr x))]
                  (call-expr cast-impl
                    (call-expr signed (literal-expr 32))
                    (call-expr * (name-expr x^) (name-expr N))))))

  (component C8
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c C7 (literal-expr 10))
    (assignment (field-expr (name-expr c) x) (slot-expr (name-expr x)))
    (assignment (name-expr y) (slot-expr (field-expr (name-expr c) y))))

  (define c8-inst (C8-make))
  (slot-set! (c8-inst x) (signal 10 20 30 40 50))

  (test-case "Can instantiate a component"
    (check-sig-equal? (slot-ref c8-inst y) (signal 100 200 300 400 500) 5))

  (component C9
    (data-port x0 in (call-expr signed (literal-expr 32)))
    (data-port x1 in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c (multiplicity (literal-expr 2)) C7 (literal-expr 10))
    (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 0)) x) (slot-expr (name-expr x0)))
    (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 1)) x) (slot-expr (name-expr x1)))
    (assignment (name-expr y)
      (lift-expr [y0 (slot-expr (field-expr (indexed-expr (name-expr c) (literal-expr 0)) y))]
                 [y1 (slot-expr (field-expr (indexed-expr (name-expr c) (literal-expr 1)) y))]
        (call-expr cast-impl
          (call-expr signed (literal-expr 32))
          (call-expr + (name-expr y0) (name-expr y1))))))

  (define c9-inst (C9-make))
  (slot-set! (c9-inst x0) (signal 10 20 30 40 50))
  (slot-set! (c9-inst x1) (signal 1 2 3 4 5))

  (test-case "Can instantiate a multiple component"
    (check-sig-equal? (slot-ref c9-inst y) (signal 110 220 330 440 550) 5))

  (component C10
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (register-expr (literal-expr 0) (slot-expr (name-expr x)))))

  (define c10-inst (C10-make))
  (slot-set! (c10-inst x) (signal 10 20 30 40 50))

  (test-case "Can register a signal"
    (check-sig-equal? (slot-ref c10-inst y) (signal 0  10 20 30 40 50) 6))

  (component C11
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (register-expr (literal-expr 0) (when-clause (slot-expr (name-expr x)))
                                             (slot-expr (name-expr y)))))

  (define c11-inst (C11-make))
  (slot-set! (c11-inst x) (signal #f #f #f #t #f))
  (slot-set! (c11-inst y) (signal 10 20 30 40 50))

  (test-case "Can register a signal with reset"
    (check-sig-equal? (slot-ref c11-inst z) (signal 0  10 20 30 0  50) 6))

  (component C12
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (register-expr (literal-expr 0)
                                             (slot-expr (name-expr y)) (when-clause (slot-expr (name-expr x))))))

  (define c12-inst (C12-make))
  (slot-set! (c12-inst x) (signal #f #t #f #t #f))
  (slot-set! (c12-inst y) (signal 10 20 30 40 50))

  (test-case "Can register a signal with enable"
    (check-sig-equal? (slot-ref c12-inst z) (signal 0  0  20 20 40) 6))

  (component C13
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u out (call-expr signed (literal-expr 32)))
    (assignment (name-expr u)
      (register-expr (literal-expr 0) (when-clause (slot-expr (name-expr x)))
                     (slot-expr (name-expr z)) (when-clause (slot-expr (name-expr y))))))

  (define c13-inst (C13-make))
  (slot-set! (c13-inst x) (signal #f #f #t #f #f))
  (slot-set! (c13-inst y) (signal #f #t #f #t #f))
  (slot-set! (c13-inst z) (signal 10 20 30 40 50))

  (test-case "Can register a signal with reset and enable"
    (check-sig-equal? (slot-ref c13-inst u) (signal 0  0  20 0  40) 6))

  (component C14
    (constant N (literal-expr 56))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (signal-expr (name-expr N))))

  (define c14-inst (C14-make))

  (test-case "Can read a local constant"
    (check-sig-equal? (slot-ref c14-inst y) (signal 56) 1))

  (test-case "Can read a constant as a channel field"
    (check-equal? (slot-ref c14-inst N) 56))

  (interface I8
    (constant N (literal-expr 56))
    (data-port y out (call-expr signed (literal-expr 32))))

  (component C15
    (composite-port p I8)
    (assignment (field-expr (name-expr p) y) (signal-expr (field-expr (name-expr p) N))))

  (define c15-inst (C15-make))

  (test-case "Can read a constant from a port"
    (check-sig-equal? (slot-ref c15-inst p y) (signal 56) 1))

  (component C16
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c C14)
    (assignment (name-expr y) (signal-expr (field-expr (name-expr c) N))))

  (define c16-inst (C16-make))

  (test-case "Can read a constant from an instance"
    (check-sig-equal? (slot-ref c16-inst y) (signal 56) 1))

  (component C17
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c C15)
    (assignment (name-expr y) (signal-expr (field-expr (field-expr (name-expr c) p) N))))

  (define c17-inst (C17-make))

  (test-case "Can read a constant from an instance port"
    (check-sig-equal? (slot-ref c17-inst y) (signal 56) 1))

  (constant K0 (literal-expr 44))

  (component C18
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (signal-expr (name-expr K0 -constant))))

  (define c18-inst (C18-make))

  (test-case "Can read a global constant"
    (check-sig-equal? (slot-ref c18-inst y) (signal 44) 1))

  (component C19
    (constant N (literal-expr 255))
    (data-port x in (call-expr signed (literal-expr 32)))
    (local-signal y (name-expr x))
    (local-signal z (name-expr N)))

  (define c19-inst (C19-make))

  (test-case "Can infer the type of a local signal that copies a port"
    (check-equal? (slot-type (dict-ref c19-inst 'y)) (slot-type (dict-ref c19-inst 'x))))

  (test-case "Can infer the type of a local signal that copies a constant"
    (check-equal? (slot-type (dict-ref c19-inst 'y)) (slot-type (dict-ref c19-inst 'x)))
    (check-equal? (slot-type (dict-ref c19-inst 'z)) (static-data 255 (unsigned 8))))

  (component C20
    (data-port x in  (call-expr signed (literal-expr 4)))
    (data-port y in  (call-expr signed (literal-expr 4)))
    (data-port z out (call-expr signed (literal-expr 8)))
    (assignment (name-expr z)
      (lift-expr [x^ (slot-expr (name-expr x))]
                 [y^ (slot-expr (name-expr y))]
        (call-expr kw-concat-impl (name-expr x^) (call-expr signed (literal-expr 4))
                                  (name-expr y^) (call-expr signed (literal-expr 4))))))

  (define c20-inst (C20-make))
  (slot-set! (c20-inst x) (signal 0 5 -2))
  (slot-set! (c20-inst y) (signal 0 3 -4))

  (test-case "Can concatenate two integers"
    (check-sig-equal? (slot-ref c20-inst z) (signal 0 83 -20) 3))

  (component C21
    (data-port x in  (call-expr unsigned (literal-expr 8)))
    (data-port y out (call-expr unsigned (literal-expr 8)))
    (assignment (name-expr y) (slot-expr (name-expr u)))
    (local-signal u (register-expr (literal-expr 0) (slot-expr (name-expr s))))
    (local-signal s (register-expr (literal-expr 0) (slot-expr (name-expr x)))))

  (define c21-inst (C21-make))

  (test-case "Can infer types when assignments are in reverse order"
    (check-equal? (actual-type (slot-type (dict-ref c21-inst 's))) (actual-type (slot-type (dict-ref c21-inst 'x))))
    (check-equal? (actual-type (slot-type (dict-ref c21-inst 'u))) (actual-type (slot-type (dict-ref c21-inst 's))))))
