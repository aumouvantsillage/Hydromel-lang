#lang racket

(require
  syntax/parse/define
  racket/stxparam
  racket/splicing
  "signal.rkt"
  "std.rkt"
  "logic-vector.rkt"
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
  connection
  literal-expr
  alias
  name-expr
  field-expr
  indexed-expr
  call-expr
  register-expr
  when-clause
  signal-expr
  static-expr
  lift-expr)

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
    (define/syntax-parse ((parameter name _ ...) ...) (stx-filter stx-lst '(parameter)))
    (attribute name))

  ; Return the list of port and signal names in the given syntax object.
  (define (design-unit-field-names stx-lst)
    (define/syntax-parse ((_ name _ ...) ...) (design-unit-fields stx-lst))
    (attribute name))

  ; Generate type inference code.
  ; TODO defer type inference to support out-of-order dependencies
  ; TODO handle circular dependencies in register-expr
  (define (type-inference stx)
    (syntax-parse stx
      #:literals [name-expr register-expr when-clause field-expr call-expr signal-expr lift-expr]
      [(name-expr _)
       (quasisyntax/loc stx
         (let ([x #,stx])
           (match x
             [(slot _ _)            (slot-type x)]
             [(logic-vector _ n #f) (unsigned n)]
             [(logic-vector _ n #t) (signed   n)]
             [_                     'any])))]

      [(register-expr _ (~optional (when-clause _)) d (~optional (when-clause _)))
       (type-inference #'d)]

      [(field-expr expr field-name)
       (syntax/loc stx
         (slot-type (dict-ref expr 'field-name)))]

      [(call-expr name arg ...)
       #:with sig-name (format-id #'here "~a-signature" #'name)
       #:with (arg-type ...) (map type-inference (attribute arg))
       (syntax/loc stx
         (sig-name arg-type ...))]

      ; TODO
      ; [(field-expr expr field-name type-name)]

      [(signal-expr x)
       (type-inference #'x)]

      [(lift-expr (name sexpr) ...+ expr)
       #:with (sexpr-type ...) (map type-inference (attribute sexpr))
       #:with expr-type (type-inference #'expr)
       (syntax/loc stx
         (let ([name (slot #f (type-thunk sexpr-type))] ...)
           expr-type))]

      [_ #''any])))

(define-syntax (import stx)
  (raise-syntax-error #f "should not be used outside of begin-tiny-hdl" stx))

; An interface or a component expands to a constructor function
; that returns a hash-map with public or debug data.
; Interfaces and components differ by the element types they are allowed
; to contain, but the expansion rule is exactly the same.
(define-syntax-parse-rule (design-unit name body ...)
  #:with ctor-name        (design-unit-ctor-name #'name)
  #:with (param-name ...) (design-unit-parameter-names (attribute body))
  #:with (field-name ...) (design-unit-field-names     (attribute body))
  (begin
    (provide ctor-name)
    (define (ctor-name param-name ...)
      body ...
      (make-hash `((field-name . ,field-name) ...)))))

(define-syntax-parse-rule (interface name body ...)
  (design-unit name body ...))

(define-syntax-parse-rule (component name body ...)
  (design-unit name body ...))

; Parameters are expanded in macro design-unit.
; TODO add type checking
(define-syntax-parse-rule (parameter _ ...)
  (begin))

; A data port expands to a variable containing an empty slot.
; The slot is relevant for input ports because they are assigned from outside
; the current component instance.
; We use a slot for output ports as well to keep a simple port access mechanism.
(define-syntax-parse-rule (data-port name _ type)
  (define name (slot #f (type-thunk type))))

; A local signal expands to a variable containing the result of the given
; expression in a slot.
; Like output ports, local signals do not need to be wrapped in a slot, but
; it helps expanding expressions without managing several special cases.
; TODO optional type
(define-syntax-parse-rule (local-signal name (~optional type) expr)
  #:with type^ (or (attribute type) (type-inference #'expr))
  (define name (slot expr (type-thunk type^))))

; Multiplicity indications are processed in macros composite-port and instance.
(define-syntax (multiplicity stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; Splice mode indication is processed in macro composite-port.
(define-syntax (splice stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; Flip mode indication is processed in macro composite-port.
(define-syntax (flip stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; A composite port expands to a variable that stores the result of a constructor
; call for the corresponding interface.
; If the multiplicity is greater than 1, a vector of channels is created.
(define-syntax-parse-rule (composite-port name (~optional (multiplicity mult)) (~or (~literal splice) (~literal flip)) ... intf-name arg ...)
  #:with ctor-name (design-unit-ctor-name #'intf-name)
  #:with m (if (attribute mult) #'(logic-vector-value mult) #'1)
  (define name (let ([ctor (λ (z) (ctor-name arg ...))])
                 (if (> m 1)
                   (build-vector m ctor)
                   (ctor #f)))))

; An instance expands to a variable that stores the result of a constructor call
; for the corresponding component.
; If the multiplicity is greater than 1, a vector of channels is created.
(define-syntax-parse-rule (instance name (~optional ((~literal multiplicity) mult)) comp-name arg ...)
  #:with ctor-name (design-unit-ctor-name #'comp-name)
  #:with m (if (attribute mult) #'(logic-vector-value mult) #'1)
  (define name (let ([ctor (λ (z) (ctor-name arg ...))])
                 (if (> m 1)
                   (build-vector m ctor)
                   (ctor #f)))))

; An if statement expands to a conditional statement that generates a hash map.
; That hash map is assigned to a variable with the same name as the if label.
(define-syntax-parse-rule (if-statement name (~seq condition then-body) ... else-body)
  (define name (cond [(logic-vector-true? condition) then-body]
                     ...
                     [else else-body])))

(define-syntax-parse-rule (for-statement name iter-name expr body ...)
  (define name (for/vector ([iter-name (in-list expr)])
                 body ...)))

; A statement block executes statements and returns a hash map that exposes
; local data for debugging.
(define-syntax-parse-rule (statement-block body ...)
  #:with (field-name ...) (design-unit-field-names (attribute body))
  (let ()
    body ...
    (make-hash `((field-name . ,field-name) ...))))

; A constant expands to a variable definition.
(define-syntax-parser constant
  ; In an internal definition context, expand to a define.
  [(constant name expr) #:when (list? (syntax-local-context))
   #'(define name expr)]
  ; In a module-level context, expand to a suffixed define.
  [(constant name expr)
   #:with name^ (format-id #'name "~a-constant" #'name)
   #'(begin
       (provide name^)
       (define name^ expr))])

; An alias expands to a partial access to the target port.
; The alias and the corresponding port must refer to the same slot.
(define-syntax-parse-rule (alias name port-name)
  (define name (dict-ref port-name 'name)))

; An assignment fills the target port's slot with the signal
; from the right-hand side.
(define-syntax-parse-rule (assignment target expr)
  (set-slot-signal! target expr))

(define-syntax-parse-rule (connection left right)
  (hydromel-connect left right))

; A literal expression expands to its value.
(define-syntax-parse-rule (literal-expr value)
  (make-logic-vector value))

; A name expression expands to the corresponding variable name in the current scope.
(define-syntax-parser name-expr
  [(name-expr name) #'name]
  [(name-expr name suffix) (format-id #'name "~a~a" #'name #'suffix)])

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
   #'(vector-ref (indexed-expr expr index ...) (logic-vector-value last))]
  [(indexed-expr expr)
   #'expr])

; A call expression expands to a Racket function call.
(define-syntax-parse-rule (call-expr fn-name arg ...)
  (fn-name arg ...))

; When clauses are processed in macro register-expr.
(define-syntax (when-clause stx)
  (raise-syntax-error #f "should be used inside a register expression" stx))

(define-syntax-parser register-expr
  #:literals [when-clause]
  [(_ i (when-clause r) d (when-clause e)) #'(register/re i r e d)]
  [(_ i (when-clause r) d)                 #'(register/r  i r   d)]
  [(_ i d (when-clause e))                 #'(register/e  i   e d)]
  [(_ i d)                                 #'(register    i     d)])

; A signal expression is a wrapper element added by the semantic checker to
; identify an expression that refers to a port or local signal for reading.
; It expands to a deferred signal read.
(define-syntax-parse-rule (signal-expr expr)
  (signal-defer (slot-signal expr)))

; A static expression wraps a constant into a signal.
(define-syntax-parse-rule (static-expr expr)
  (signal expr))

; A lift expression is a wrapper element added by the semantic checker
; when an expression depends on some signal values.
; name ... is a list of signal names that are needed to compute expr.
(define-syntax-parser lift-expr
  #:literals [signal-expr]
  ; Lift a signal expression. Since the expression returns a signal,
  ; we must lift signal-first to avoid created a signal of signals.
  ; This is typically used when indexed-expr contains signals as indices.
  [(_ binding ...+ (signal-expr expr))
   #'(lift-expr binding ... (signal-first (signal-expr expr)))]
  ; Lift any expression that computes values from values.
  ; expr must not contain elements of type signal-expr.
  [(_ (name sexpr) ...+ expr)
   #'(for/signal ([name sexpr] ...) expr)])

; TODO test functions
(module+ test
  (require
    rackunit
    "helpers.rkt"
    "signal.rkt"
    "types.rkt")

  (interface I0
    (parameter N (name-expr unsigned))
    (data-port x in  (call-expr signed (name-expr N)))
    (data-port y out (call-expr signed (name-expr N))))

  (component C0
    (parameter N (name-expr unsigned))
    (data-port x in  (call-expr signed (name-expr N)))
    (data-port y out (call-expr signed (name-expr N)))
    (assignment (name-expr y) (signal-expr (name-expr x))))

  (interface I1
    (composite-port i I0 8)
    (data-port z in (call-expr signed (literal-expr 32))))

  (component C1
    (composite-port i I0 8)
    (data-port z in (call-expr signed (literal-expr 32)))
    (assignment (field-expr (name-expr i) y) (signal-expr (field-expr (name-expr i) x))))

  (interface I2
    (data-port x in  (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32))))

  (interface I3
    (data-port z in (call-expr signed (literal-expr 32)))
    (composite-port i I2)
    (data-port u out (call-expr signed (literal-expr 32))))

  (interface I4
    (data-port v in (call-expr signed (literal-expr 32)))
    (composite-port j I3)
    (data-port w out (call-expr signed (literal-expr 32))))

  (component C2
    (data-port v in (call-expr signed (literal-expr 32)))
    (composite-port j I3)
    (data-port w out (call-expr signed (literal-expr 32)))
    (assignment (name-expr w) (signal-expr (name-expr v))))

  (interface I5
    (composite-port i (multiplicity (literal-expr 3)) I2))

  (interface I6
    (parameter N (name-expr unsigned))
    (composite-port i (multiplicity (name-expr N)) I2))

  (interface I7
    (parameter M (name-expr unsigned))
    (composite-port j I6 (name-expr M)))

  (component C3
    (composite-port i I2)
    (assignment (field-expr (name-expr i) y)
                (signal-expr (field-expr (name-expr i) x))))

  (component C4
    (composite-port i (multiplicity (literal-expr 3)) I2)
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 2)) y)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 2)) x))))

  (interface I8
    (data-port x in (call-expr signed (literal-expr 32))))

  (component C5
    (composite-port i (multiplicity (literal-expr 3)) I8)
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z)
                (lift-expr [y^ (signal-expr (name-expr y))]
                           (signal-expr (field-expr (indexed-expr (name-expr i) (name-expr y^)) x)))))

  (component C6
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z)
                (lift-expr [x^ (signal-expr (name-expr x))]
                           [y^ (signal-expr (name-expr y))]
                           (call-expr logic-vector-+ (name-expr x^) (name-expr y^)))))

  (component C7
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u in (call-expr signed (literal-expr 32)))
    (data-port v out (call-expr signed (literal-expr 32)))
    (local-signal xy (lift-expr [x^ (signal-expr (name-expr x))]
                                [y^ (signal-expr (name-expr y))]
                                (call-expr logic-vector-* (name-expr x^) (name-expr y^))))
    (local-signal zu (lift-expr [z^ (signal-expr (name-expr z))]
                                [u^ (signal-expr (name-expr u))]
                                (call-expr logic-vector-* (name-expr z^) (name-expr u^))))
    (assignment (name-expr v)
                (lift-expr [xy^ (signal-expr (name-expr xy))]
                           [zu^ (signal-expr (name-expr zu))]
                           (call-expr logic-vector-+ (name-expr xy^) (name-expr zu^)))))

  (component C8
    (parameter N (name-expr unsigned))
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y)
                (lift-expr [x^ (signal-expr (name-expr x))]
                           (call-expr logic-vector-* x^ N))))
  (component C9
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c C8 (literal-expr 10))
    (assignment (field-expr (name-expr c) x) (signal-expr (name-expr x)))
    (assignment (name-expr y) (signal-expr (field-expr (name-expr c) y))))

  (component C10
    (data-port x0 in (call-expr signed (literal-expr 32)))
    (data-port x1 in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c (multiplicity (literal-expr 2)) C8 (literal-expr 10))
    (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 0)) x) (signal-expr (name-expr x0)))
    (assignment (field-expr (indexed-expr (name-expr c) (literal-expr 1)) x) (signal-expr (name-expr x1)))
    (assignment (name-expr y) (lift-expr [y0 (signal-expr (field-expr (indexed-expr (name-expr c) (literal-expr 0)) y))]
                                         [y1 (signal-expr (field-expr (indexed-expr (name-expr c) (literal-expr 1)) y))]
                                         (call-expr logic-vector-+ y0 y1))))

  (component C11
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (register-expr (literal-expr 0) (signal-expr (name-expr x)))))

  (component C12
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (register-expr (literal-expr 0) (when-clause (signal-expr (name-expr x)))
                                             (signal-expr (name-expr y)))))

  (component C13
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (register-expr (literal-expr 0)
                                             (signal-expr (name-expr y)) (when-clause (signal-expr (name-expr x))))))

  (component C14
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u out (call-expr signed (literal-expr 32)))
    (assignment (name-expr u) (register-expr (literal-expr 0) (when-clause (signal-expr (name-expr x)))
                                             (signal-expr (name-expr z)) (when-clause (signal-expr (name-expr y))))))

  (component C15
    (constant N (literal-expr 56))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (signal (name-expr N))))

  (interface I9
    (constant N (literal-expr 56))
    (data-port y out (call-expr signed (literal-expr 32))))

  (component C16
    (composite-port p I9)
    (assignment (field-expr (name-expr p) y) (signal (field-expr (name-expr p) N))))

  (component C17
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c C15)
    (assignment (name-expr y) (signal (field-expr (name-expr c) N))))

  (component C18
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c C16)
    (assignment (name-expr y) (signal (field-expr (field-expr (name-expr c) p) N))))

  (constant K (literal-expr 44))

  (component C19
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (signal (name-expr K -constant))))

  (component C20
    (constant N (literal-expr 255))
    (data-port x in (call-expr signed (literal-expr 32)))
    (local-signal y (name-expr x))
    (local-signal z (name-expr N)))


  (define (check-sig-equal? t e n)
    (check-equal? (logic-signal-take t n) (logic-signal-take e n)))

  (define .+ (signal-lift logic-vector-+))
  (define .* (signal-lift logic-vector-*))

  (test-case "Can construct a channel for an interface with simple ports"
    (define i (I0-make 30))
    (check-pred slot? (dict-ref i 'x))
    (check-pred slot? (dict-ref i 'y)))

  (test-case "Can construct a channel for a component with simple ports"
    (define c (C0-make 30))
    (check-pred slot? (dict-ref c 'x))
    (check-pred slot? (dict-ref c 'y)))

  (test-case "Can construct a channel for an interface with composite ports and no parameters"
    (define i3 (I3-make))
    (check-pred slot? (dict-ref i3 'z))
    (check-pred dict? (dict-ref i3 'i))
    (check-pred slot? (dict-ref (dict-ref i3 'i) 'x))
    (check-pred slot? (dict-ref (dict-ref i3 'i) 'y))
    (check-pred slot? (dict-ref i3 'u))

    (define i4 (I4-make))
    (check-pred slot? (dict-ref i4 'v))
    (check-pred dict? (dict-ref i4 'j))
    (check-pred slot? (dict-ref (dict-ref i4 'j) 'z))
    (check-pred slot? (dict-ref (dict-ref (dict-ref i4 'j) 'i) 'x))
    (check-pred slot? (dict-ref (dict-ref (dict-ref i4 'j) 'i) 'y))
    (check-pred slot? (dict-ref (dict-ref i4 'j) 'u))
    (check-pred slot? (dict-ref i4 'w)))

  (test-case "Can construct a channel for a component with composite ports and no parameters"
    (define c (C2-make))
    (check-pred slot? (dict-ref c 'v))
    (check-pred dict? (dict-ref c 'j))
    (check-pred slot? (dict-ref (dict-ref c 'j) 'z))
    (check-pred slot? (dict-ref (dict-ref (dict-ref c 'j) 'i) 'x))
    (check-pred slot? (dict-ref (dict-ref (dict-ref c 'j) 'i) 'y))
    (check-pred slot? (dict-ref (dict-ref c 'j) 'u))
    (check-pred slot? (dict-ref c 'w)))

  (test-case "Can construct a channel for an interface with a vector port"
    (define j (I5-make))
    (check-pred vector? (dict-ref j 'i))
    (check-eq? (vector-length (dict-ref j 'i)) 3)
    (for ([n (range 3)])
      (check-pred dict? (vector-ref (dict-ref j 'i) n))))

  (test-case "Can construct a channel for an interface with arguments"
    (define j (I6-make (make-logic-vector 5)))
    (check-pred vector? (dict-ref j 'i))
    (check-eq? (vector-length (dict-ref j 'i)) 5)
    (for ([n (range 5)])
      (check-pred dict? (vector-ref (dict-ref j 'i) n))))

  (test-case "Can construct a channel containing a composite port with arguments"
    (define k (I7-make (make-logic-vector 3)))
    (check-pred vector? (dict-ref (dict-ref k 'j) 'i))
    (check-eq? (vector-length (dict-ref (dict-ref k 'j) 'i)) 3)
    (for ([n (range 3)])
      (check-pred dict? (vector-ref (dict-ref (dict-ref k 'j) 'i) n))))

  (test-case "Can assign a simple port to another simple port"
    (define c (C0-make 32))
    (define x (logic-signal 23))
    (port-set! (c x) x)
    (check-sig-equal? (port-ref c y) x 5))

  (test-case "Can access simple ports in a composite port"
    (define c (C3-make))
    (define x (logic-signal 23))
    (port-set! (c i x) x)
    (check-sig-equal? (port-ref c i y) x 5))

  (test-case "Can access simple ports in a vector composite port with static indices"
    (define c (C4-make))
    (define x0 (logic-signal 10))
    (define x1 (logic-signal 20))
    (define x2 (logic-signal 30))
    (port-set! (c i 0 x) x0)
    (port-set! (c i 1 x) x1)
    (port-set! (c i 2 x) x2)
    (check-sig-equal? (port-ref c i 0 x) x0 5)
    (check-sig-equal? (port-ref c i 1 x) x1 5)
    (check-sig-equal? (port-ref c i 2 x) x2 5))

  (test-case "Can access simple ports in a vector composite port with dynamic indices"
    (define c (C5-make))
    (define x0 (logic-signal 10))
    (define x1 (logic-signal 20))
    (define x2 (logic-signal 30))
    (define y (logic-signal 0 1 2 1 0 2))
    (port-set! (c i 0 x) x0)
    (port-set! (c i 1 x) x1)
    (port-set! (c i 2 x) x2)
    (port-set! (c y)     y)
    (define z (logic-signal 10 20 30 20 10 30))
    (check-sig-equal? (port-ref c z) z 5))

  (test-case "Can perform an operation between signals"
    (define c (C6-make))
    (define x (logic-signal 1  2  3  4  5))
    (define y (logic-signal 10 20 30 40 50))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (check-sig-equal? (port-ref c z) (.+ x y) 5))

  (test-case "Can use local signals"
    (define c (C7-make))
    (define x (logic-signal 10 20 30 40 50))
    (define y (logic-signal 2))
    (define z (logic-signal 1 2 3 4 5))
    (define u (logic-signal 3))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (port-set! (c z) z)
    (port-set! (c u) u)
    (check-sig-equal? (port-ref c v) (.+ (.* x y) (.* z u)) 5))

  (test-case "Can instantiate a component"
    (define c (C9-make))
    (define x (logic-signal 10 20 30 40 50))
    (port-set! (c x) x)
    (check-sig-equal? (port-ref c y) (.* x (logic-signal 10)) 5))

  (test-case "Can instantiate a multiple component"
    (define c (C10-make))
    (define x0 (logic-signal 10 20 30 40 50))
    (define x1 (logic-signal 1 2 3 4 5))
    (port-set! (c x0) x0)
    (port-set! (c x1) x1)
    (check-sig-equal? (port-ref c y) (.* (.+ x0 x1) (logic-signal 10)) 5))

  (test-case "Can register a signal"
    (define c (C11-make))
    (define x (logic-signal 10 20 30 40 50))
    (define y (logic-signal 0  10 20 30 40 50))
    (port-set! (c x) x)
    (check-sig-equal? (port-ref c y) y 6))

  (test-case "Can register a signal with reset"
    (define c (C12-make))
    (define x (signal       #f #f #f #t #f))
    (define y (logic-signal 10 20 30 40 50))
    (define z (logic-signal 0  10 20 30 0  50))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (check-sig-equal? (port-ref c z) z 6))

  (test-case "Can register a signal with enable"
    (define c (C13-make))
    (define x (signal       #f #t #f #t #f))
    (define y (logic-signal 10 20 30 40 50))
    (define z (logic-signal 0  0  20 20 40))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (check-sig-equal? (port-ref c z) z 6))

  (test-case "Can register a signal with reset and enable"
    (define c (C14-make))
    (define x (signal       #f #f #t #f #f))
    (define y (signal       #f #t #f #t #f))
    (define z (logic-signal 10 20 30 40 50))
    (define u (logic-signal 0  0  20 0  40))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (port-set! (c z) z)
    (check-sig-equal? (port-ref c u) u 6))

  (test-case "Can read a local constant"
    (define c (C15-make))
    (check-sig-equal? (port-ref c y) (logic-signal 56) 1))

  (test-case "Can read a constant as a channel field"
    (define c (C15-make))
    (check-equal? (logic-vector-value (dict-ref c 'N)) 56))

  (test-case "Can read a constant from a port"
    (define c (C16-make))
    (check-sig-equal? (port-ref c p y) (logic-signal 56) 1))

  (test-case "Can read a constant from an instance"
    (define c (C17-make))
    (check-sig-equal? (port-ref c y) (logic-signal 56) 1))

  (test-case "Can read a constant from an instance port"
    (define c (C18-make))
    (check-sig-equal? (port-ref c y) (logic-signal 56) 1))

  (test-case "Can read a global constant"
    (define c (C19-make))
    (check-sig-equal? (port-ref c y) (logic-signal 44) 1))

  (test-case "Can infer the type of a port"
    (define c (C0-make 30))
    (check-equal? (slot-type (dict-ref c 'x)) (signed 30)))

  (test-case "Can infer the type of a local signal that copies a port"
    (define c (C20-make))
    (check-equal? (slot-type (dict-ref c 'y)) (slot-type (dict-ref c 'x))))

  (test-case "Can infer the type of a local signal that copies a constant"
    (define c (C20-make))
    (check-equal? (slot-type (dict-ref c 'y)) (slot-type (dict-ref c 'x)))
    (check-equal? (slot-type (dict-ref c 'z)) (unsigned 8))))
