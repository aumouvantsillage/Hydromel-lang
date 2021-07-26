#lang racket

(require
  syntax/parse/define
  racket/stxparam
  racket/splicing
  "signal.rkt"
  "std.rkt"
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
  statement-block
  assignment
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

  ; Return the list of ports, signals and instances in the given syntax object.
  (define (design-unit-fields stx-lst)
    (stx-filter stx-lst '(constant data-port composite-port local-signal instance if-statement)))

  ; Return the list of parameter names in the given syntax object.
  (define (design-unit-parameter-names stx-lst)
    (define/syntax-parse ((parameter name _ ...) ...) (stx-filter stx-lst '(parameter)))
    (attribute name))

  ; Return the list of port and signal names in the given syntax object.
  (define (design-unit-field-names stx-lst)
    (define/syntax-parse ((_ name _ ...) ...) (design-unit-fields stx-lst))
    (attribute name)))

(define-syntax (import stx)
  (raise-syntax-error #f "should not be used outside of begin-tiny-hdl" stx))

; An interface or a component expands to a constructor function
; that returns a hash-map with public or debug data.
; Interfaces and components differ by the element types they are allowed
; to contain, but the expansion rule is exactly the same.
(define-syntax-parse-rule (design-unit name body ...)
  #:with (param-name ...) (design-unit-parameter-names (attribute body))
  #:with (field-name ...) (design-unit-field-names     (attribute body))
  (define (name param-name ...)
    body ...
    (make-immutable-hash `((field-name . ,field-name) ...))))

(define-syntax-parse-rule (interface name body ...)
  (design-unit name body ...))

(define-syntax-parse-rule (component name body ...)
  (design-unit name body ...))

; Parameters are expanded in macro design-unit.
(define-syntax-parse-rule (parameter _ ...)
  (begin))

; A data port expands to a variable containing an empty box.
; The box is relevant for input ports because they are assigned from outside
; the current component instance.
; We use a box for output ports as well to keep a simple port access mechanism.
(define-syntax-parse-rule (data-port name _ ...)
  (define name (box #f)))

; A local signal expands to a variable containing the result of the given
; expression in a box.
; Like output ports, local signals do not need to be wrapped in a box, but
; it helps expanding expressions without managing several special cases.
(define-syntax-parse-rule (local-signal name expr)
  (define name (box expr)))

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
  #:with m (or (attribute mult) #'1)
  (define name (let ([ctor (λ (z) (intf-name arg ...))])
                 (if (> m 1)
                   (build-vector m ctor)
                   (ctor #f)))))

; An instance expands to a variable that stores the result of a constructor call
; for the corresponding component.
; If the multiplicity is greater than 1, a vector of channels is created.
(define-syntax-parse-rule (instance name (~optional ((~literal multiplicity) mult)) comp-name arg ...)
   #:with m (or (attribute mult) #'1)
   (define name (let ([ctor (λ (z) (comp-name arg ...))])
                  (if (> m 1)
                    (build-vector m ctor)
                    (ctor #f)))))

; An if statement expands to a conditional statement that generates a hash map.
; That hash map is assigned to a variable with the same name as the if label.
(define-syntax-parse-rule (if-statement name (~seq condition then-body) ... else-body)
  (define name (cond [(not (zero? condition)) then-body]
                     ...
                     [else else-body])))

; A statement block executes statements and returns a hash map that exposes
; local data for debugging.
(define-syntax-parse-rule (statement-block body ...)
  #:with (field-name ...) (design-unit-field-names (attribute body))
  (let ()
    body ...
    (make-immutable-hash `((field-name . ,field-name) ...))))

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

; An alias expands to a variable that receives the value of the target port.
(define-syntax-parse-rule (alias name port-name)
  (define name (field-expr (name-expr port-name) name)))

; An assignment fills the target port's box with the signal
; from the right-hand side.
(define-syntax-parse-rule (assignment target expr)
  (set-box! target expr))

; A literal expression expands to its value.
(define-syntax-parse-rule (literal-expr value)
  value)

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
   #'(vector-ref (indexed-expr expr index ...) last)]
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
  (signal-defer (unbox expr)))

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
    "signal.rkt")

  (interface I0
    (parameter N #f)
    (data-port x in  #f)
    (data-port y out #f))

  (component C0
    (parameter N #f)
    (data-port x in  #f)
    (data-port y out #f)
    (assignment (name-expr y) (signal-expr (name-expr x))))

  (interface I1
    (composite-port i I0)
    (data-port z in #f))

  (component C1
    (composite-port i I0)
    (data-port z in #f)
    (assignment (field-expr (name-expr i) y) (signal-expr (field-expr (name-expr i) x))))

  (interface I2
    (data-port x in #f)
    (data-port y out #f))

  (interface I3
    (data-port z in #f)
    (composite-port i I2)  (data-port u out #f))

  (interface I4
    (data-port v in #f)
    (composite-port j I3)
    (data-port w out #f))

  (component C2
    (data-port v in #f)
    (composite-port j I3)
    (data-port w out #f)
    (assignment (name-expr w) (signal-expr (name-expr v))))

  (interface I5
    (composite-port i (multiplicity 3) I2))

  (interface I6
    (parameter N #f)
    (composite-port i (multiplicity (name-expr N)) I2))

  (interface I7
    (parameter M #f)
    (composite-port j I6 (name-expr M)))

  (component C3
    (composite-port i I2)
    (assignment (field-expr (name-expr i) y)
                (signal-expr (field-expr (name-expr i) x))))

  (component C4
    (composite-port i (multiplicity 3) I2)
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 2)) y)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 2)) x))))

  (interface I8
    (data-port x in (name-expr integer)))

  (component C5
    (composite-port i (multiplicity (literal-expr 3)) I8)
    (data-port y in (name-expr integer))
    (data-port z out (name-expr integer))
    (assignment (name-expr z)
                (lift-expr [y^ (signal-expr (name-expr y))]
                           (signal-expr (field-expr (indexed-expr (name-expr i) (name-expr y^)) x)))))

  (component C6
    (data-port x in #f)
    (data-port y in #f)
    (data-port z out #f)
    (assignment (name-expr z)
                (lift-expr [x^ (signal-expr (name-expr x))]
                           [y^ (signal-expr (name-expr y))]
                           (call-expr + (name-expr x^) (name-expr y^)))))

  (component C7
    (data-port x in #f)
    (data-port y in #f)
    (data-port z in #f)
    (data-port u in #f)
    (data-port v out #f)
    (local-signal xy (lift-expr [x^ (signal-expr (name-expr x))]
                                [y^ (signal-expr (name-expr y))]
                                (call-expr * (name-expr x^) (name-expr y^))))
    (local-signal zu (lift-expr [z^ (signal-expr (name-expr z))]
                                [u^ (signal-expr (name-expr u))]
                                (call-expr * (name-expr z^) (name-expr u^))))
    (assignment (name-expr v)
                (lift-expr [xy^ (signal-expr (name-expr xy))]
                           [zu^ (signal-expr (name-expr zu))]
                           (call-expr + (name-expr xy^) (name-expr zu^)))))

  (component C8
    (parameter N #f)
    (data-port x in #f)
    (data-port y out #f)
    (assignment (name-expr y)
                (lift-expr [x^ (signal-expr (name-expr x))]
                           (call-expr * x^ N))))
  (component C9
    (data-port x in #f)
    (data-port y out #f)
    (instance c C8 10)
    (assignment (field-expr (name-expr c) x) (signal-expr (name-expr x)))
    (assignment (name-expr y) (signal-expr (field-expr (name-expr c) y))))

  (component C10
    (data-port x0 in #f)
    (data-port x1 in #f)
    (data-port y out #f)
    (instance c (multiplicity 2) C8 10)
    (assignment (field-expr (indexed-expr (name-expr c) 0) x) (signal-expr (name-expr x0)))
    (assignment (field-expr (indexed-expr (name-expr c) 1) x) (signal-expr (name-expr x1)))
    (assignment (name-expr y) (lift-expr [y0 (signal-expr (field-expr (indexed-expr (name-expr c) 0) y))]
                                         [y1 (signal-expr (field-expr (indexed-expr (name-expr c) 1) y))]
                                         (call-expr + y0 y1))))

  (component C11
    (data-port x in #f)
    (data-port y out #f)
    (assignment (name-expr y) (register-expr (literal-expr 0) (signal-expr (name-expr x)))))

  (component C12
    (data-port x in #f)
    (data-port y in #f)
    (data-port z out #f)
    (assignment (name-expr z) (register-expr (literal-expr 0) (when-clause (signal-expr (name-expr x)))
                                             (signal-expr (name-expr y)))))

  (component C13
    (data-port x in #f)
    (data-port y in #f)
    (data-port z out #f)
    (assignment (name-expr z) (register-expr (literal-expr 0)
                                             (signal-expr (name-expr y)) (when-clause (signal-expr (name-expr x))))))

  (component C14
    (data-port x in #f)
    (data-port y in #f)
    (data-port z in #f)
    (data-port u out #f)
    (assignment (name-expr u) (register-expr (literal-expr 0) (when-clause (signal-expr (name-expr x)))
                                             (signal-expr (name-expr z)) (when-clause (signal-expr (name-expr y))))))

  (component C15
    (constant N (literal-expr 56))
    (data-port y out #f)
    (assignment (name-expr y) (signal (name-expr N))))

  (interface I9
    (constant N (literal-expr 56))
    (data-port y out #f))

  (component C16
    (composite-port p I9)
    (assignment (field-expr (name-expr p) y) (signal (field-expr (name-expr p) N))))

  (component C17
    (data-port y out #f)
    (instance c C15)
    (assignment (name-expr y) (signal (field-expr (name-expr c) N))))

  (component C18
    (data-port y out #f)
    (instance c C16)
    (assignment (name-expr y) (signal (field-expr (field-expr (name-expr c) p) N))))

  (constant K (literal-expr 44))

  (component C19
    (data-port y out (name-expr integer))
    (assignment (name-expr y) (signal (name-expr K -constant))))

  (define (check-sig-equal? t e n)
    (check-equal? (signal-take t n) (signal-take e n)))

  (define .+ (signal-lift +))
  (define .* (signal-lift *))

  (test-case "Can construct a channel for an interface with simple ports"
    (define i (I0 30))
    (check-pred box? (dict-ref i 'x))
    (check-pred box? (dict-ref i 'y)))

  (test-case "Can construct a channel for a component with simple ports"
    (define c (C0 30))
    (check-pred box? (dict-ref c 'x))
    (check-pred box? (dict-ref c 'y)))

  (test-case "Can construct a channel for an interface with composite ports and no parameters"
    (define i3 (I3))
    (check-pred box?  (dict-ref i3 'z))
    (check-pred dict? (dict-ref i3 'i))
    (check-pred box?  (dict-ref (dict-ref i3 'i) 'x))
    (check-pred box?  (dict-ref (dict-ref i3 'i) 'y))
    (check-pred box?  (dict-ref i3 'u))

    (define i4 (I4))
    (check-pred box?  (dict-ref i4 'v))
    (check-pred dict? (dict-ref i4 'j))
    (check-pred box?  (dict-ref (dict-ref i4 'j) 'z))
    (check-pred box?  (dict-ref (dict-ref (dict-ref i4 'j) 'i) 'x))
    (check-pred box?  (dict-ref (dict-ref (dict-ref i4 'j) 'i) 'y))
    (check-pred box?  (dict-ref (dict-ref i4 'j) 'u))
    (check-pred box?  (dict-ref i4 'w)))

  (test-case "Can construct a channel for a component with composite ports and no parameters"
    (define c (C2))
    (check-pred box?  (dict-ref c 'v))
    (check-pred dict? (dict-ref c 'j))
    (check-pred box?  (dict-ref (dict-ref c 'j) 'z))
    (check-pred box?  (dict-ref (dict-ref (dict-ref c 'j) 'i) 'x))
    (check-pred box?  (dict-ref (dict-ref (dict-ref c 'j) 'i) 'y))
    (check-pred box?  (dict-ref (dict-ref c 'j) 'u))
    (check-pred box?  (dict-ref c 'w)))

  (test-case "Can construct a channel for an interface with a vector port"
    (define j (I5))
    (check-pred vector? (dict-ref j 'i))
    (check-eq? (vector-length (dict-ref j 'i)) 3)
    (for ([n (range 3)])
      (check-pred dict? (vector-ref (dict-ref j 'i) n))))

  (test-case "Can construct a channel for an interface with arguments"
    (define j (I6 5))
    (check-pred vector? (dict-ref j 'i))
    (check-eq? (vector-length (dict-ref j 'i)) 5)
    (for ([n (range 5)])
      (check-pred dict? (vector-ref (dict-ref j 'i) n))))

  (test-case "Can construct a channel containing a composite port with arguments"
    (define k (I7 3))
    (check-pred vector? (dict-ref (dict-ref k 'j) 'i))
    (check-eq? (vector-length (dict-ref (dict-ref k 'j) 'i)) 3)
    (for ([n (range 3)])
      (check-pred dict? (vector-ref (dict-ref (dict-ref k 'j) 'i) n))))

  (test-case "Can assign a simple port to another simple port"
    (define c (C0 #f))
    (define x (signal 23))
    (port-set! (c x) x)
    (check-sig-equal? (port-ref c y) x 5))

  (test-case "Can access simple ports in a composite port"
    (define c (C3))
    (define x (signal 23))
    (port-set! (c i x) x)
    (check-sig-equal? (port-ref c i y) x 5))

  (test-case "Can access simple ports in a vector composite port with static indices"
    (define c (C4))
    (define x0 (signal 10))
    (define x1 (signal 20))
    (define x2 (signal 30))
    (port-set! (c i 0 x) x0)
    (port-set! (c i 1 x) x1)
    (port-set! (c i 2 x) x2)
    (check-sig-equal? (port-ref c i 0 x) x0 5)
    (check-sig-equal? (port-ref c i 1 x) x1 5)
    (check-sig-equal? (port-ref c i 2 x) x2 5))

  (test-case "Can access simple ports in a vector composite port with dynamic indices"
    (define c (C5))
    (define x0 (signal 10))
    (define x1 (signal 20))
    (define x2 (signal 30))
    (define y (list->signal (list 0 1 2 1 0 2)))
    (port-set! (c i 0 x) x0)
    (port-set! (c i 1 x) x1)
    (port-set! (c i 2 x) x2)
    (port-set! (c y)     y)
    (define z (list->signal (list 10 20 30 20 10 30)))
    (check-sig-equal? (port-ref c z) z 5))

  (test-case "Can perform an operation between signals"
    (define c (C6))
    (define x (list->signal (range 1  5  1)))
    (define y (list->signal (range 10 50 10)))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (check-sig-equal? (port-ref c z) (.+ x y) 5))

  (test-case "Can use local signals"
    (define c (C7))
    (define x (list->signal (list 10 20 30 40 50)))
    (define y (signal 2))
    (define z (list->signal (list 1 2 3 4 5)))
    (define u (signal 3))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (port-set! (c z) z)
    (port-set! (c u) u)
    (check-sig-equal? (port-ref c v) (.+ (.* x y) (.* z u)) 5))

  (test-case "Can instantiate a component"
    (define c (C9))
    (define x (list->signal (list 10 20 30 40 50)))
    (port-set! (c x) x)
    (check-sig-equal? (port-ref c y) (.* x (signal 10)) 5))

  (test-case "Can instantiate a multiple component"
    (define c (C10))
    (define x0 (list->signal (list 10 20 30 40 50)))
    (define x1 (list->signal (list 1 2 3 4 5)))
    (port-set! (c x0) x0)
    (port-set! (c x1) x1)
    (check-sig-equal? (port-ref c y) (.* (.+ x0 x1) (signal 10)) 5))

  (test-case "Can register a signal"
    (define c (C11))
    (define x (list->signal (list 10 20  30 40 50)))
    (port-set! (c x) x)
    (check-sig-equal? (port-ref c y) (register 0 x) 6))

  (test-case "Can register a signal with reset"
    (define c (C12))
    (define x (list->signal (list #f #f  #f #t #f)))
    (define y (list->signal (list 10 20  30 40 50)))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (check-sig-equal? (port-ref c z) (register/r 0 x y) 6))

  (test-case "Can register a signal with enable"
    (define c (C13))
    (define x (list->signal (list #f #t  #f #t #f)))
    (define y (list->signal (list 10 20  30 40 50)))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (check-sig-equal? (port-ref c z) (register/e 0 x y) 6))

  (test-case "Can register a signal with reset and enable"
    (define c (C14))
    (define x (list->signal (list #f #t  #f #t #f)))
    (define y (list->signal (list #f #f  #t #f #f)))
    (define z (list->signal (list 10 20  30 40 50)))
    (port-set! (c x) x)
    (port-set! (c y) y)
    (port-set! (c z) z)
    (check-sig-equal? (port-ref c u) (register/re 0 x y z) 6))

  (test-case "Can read a local constant"
    (define c (C15))
    (check-sig-equal? (port-ref c y) (signal 56) 1))

  (test-case "Can read a constant as a channel field"
    (define c (C15))
    (check-equal? (dict-ref c 'N) 56))

  (test-case "Can read a constant from a port"
    (define c (C16))
    (check-sig-equal? (port-ref c p y) (signal 56) 1))

  (test-case "Can read a constant from an instance"
    (define c (C17))
    (check-sig-equal? (port-ref c y) (signal 56) 1))

  (test-case "Can read a constant from an instance port"
    (define c (C18))
    (check-sig-equal? (port-ref c y) (signal 56) 1))

  (test-case "Can read a global constant"
    (define c (C19))
    (check-sig-equal? (port-ref c y) (signal 44) 1)))
