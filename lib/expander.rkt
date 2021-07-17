#lang racket

(require
  syntax/parse/define
  racket/stxparam
  "signal.rkt"
  (for-syntax
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
  (define (channel-ctor-name name)
    (format-id name "~a-make-channel" name))

  (define (instance-ctor-name name)
    (format-id name "~a-make-instance" name))

  (define (accessor-name sname fname)
    (format-id sname "~a-~a" sname fname))

  (define (stx-filter stx-lst id-lst)
    (for/list ([it (in-list stx-lst)]
               #:when (member (syntax-e (stx-car it)) id-lst))
      it))

  ; Return the list of ports, signals and instances in the given syntax object.
  (define (design-unit-fields stx-lst)
    (stx-filter stx-lst '(data-port composite-port local-signal instance)))

  ; Return the list of statements in the given syntax object.
  (define (design-unit-statements stx-lst)
    (stx-filter stx-lst '(constant local-signal alias assignment)))

  ; Return the list of aliases in the given syntax object.
  (define (design-unit-aliases stx-lst)
    (stx-filter stx-lst '(alias)))

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

; An interface expands to:
; - a struct type,
; - a constructor function,
; - an accessor function for each alias (field of a spliced interface).
(define-syntax-parse-rule (interface name body ...)
  #:with struct-name      (generate-temporary          (format-id #'name "hdrml:struct:~a" #'name))
  #:with struct-ctor-name (generate-temporary          (format-id #'name "hdrml:ctor:~a"   #'name))
  #:with ctor-name        (channel-ctor-name           #'name)
  #:with (param-name ...) (design-unit-parameter-names (attribute body))
  #:with (field-name ...) (design-unit-field-names     (attribute body))
  #:with (field-stx ...)  (design-unit-fields          (attribute body))
  #:with (alias-stx ...)  (design-unit-aliases         (attribute body))
  (begin
    (provide (struct-out struct-name) ctor-name)
    (struct name (field-name ...)
      #:transparent
      #:name             struct-name
      #:constructor-name struct-ctor-name)
    (alias-accessor name alias-stx) ...
    (define (ctor-name param-name ...)
      (struct-ctor-name field-stx ...))))

(define-syntax-parse-rule (alias-accessor parent-intf-name (alias alias-name port-name alias-intf-name))
  #:with port-acc-name (accessor-name #'parent-intf-name #'port-name)
  #:with orig-acc-name (accessor-name #'alias-intf-name  #'alias-name)
  #:with acc-name      (accessor-name #'parent-intf-name #'alias-name)
  (begin
    (provide acc-name)
    (define (acc-name x)
      (orig-acc-name (port-acc-name x)))))

; Inside a component, this parameter becomes true when expanding statements.
; This is useful for constructs that are expanded twice, as fields and as statements.
(define-syntax-parameter as-statement #f)

; A component expands to:
; - the same output as for an interface
; - a function with the body of the component.
(define-syntax-parse-rule (component name body ...)
   #:with inst-ctor-name   (instance-ctor-name          #'name)
   #:with chan-ctor-name   (channel-ctor-name           #'name)
   #:with (param-name ...) (design-unit-parameter-names (attribute body))
   #:with (field-name ...) (design-unit-field-names     (attribute body))
   #:with (stmt ...)       (design-unit-statements      (attribute body))
   #:with (field-acc-name ...) (for/list ([fname (in-list (attribute field-name))])
                                 (accessor-name #'name fname))
   (begin
     (interface name body ...)
     (provide inst-ctor-name)
     (define (inst-ctor-name param-name ...)
       (define self (chan-ctor-name param-name ...))
       (define field-name (field-acc-name self)) ...
       (syntax-parameterize ([as-statement #t])
         ; (void) prevents errors when there is no statement.
         stmt ... (void))
       self)))

; Parameters are expanded in macros interface and component.
(define-syntax (parameter stx)
  (raise-syntax-error #f "should be used inside an interface or component" stx))

; In a channel constructor, a data port expands to an empty box.
(define-syntax-parse-rule (data-port _ ...)
  (box #f))

; A local signal expands to an assignment or an empty box, depending
; on whether it is treated as a statement or as a declaration.
(define-syntax-parser local-signal
  [(local-signal name expr)
   (if (syntax-parameter-value #'as-statement)
     ; Local signal assignment in a component body.
     #'(assignment name expr)
     ; Local signal initialization in a channel constructor.
     #'(box #f))])

; Multiplicity indications are processed in macros composite-port and instance.
(define-syntax (multiplicity stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; Splice mode indication is processed in macro composite-port.
(define-syntax (splice stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; Flip mode indication is processed in macro composite-port.
(define-syntax (flip stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; A composite port expands to a constructor call for the corresponding interface.
; If the multiplicity is greater than 1, a vector of channels is created.
(define-syntax-parse-rule (composite-port _ (~optional (multiplicity mult)) (~or (~literal splice) (~literal flip)) ... intf-name arg ...)
  #:with m (or (attribute mult) #'1)
  #:with chan-ctor-name (channel-ctor-name #'intf-name)
  (let ([ctor (λ (z) (chan-ctor-name arg ...))])
    (if (> m 1)
      (build-vector m ctor)
      (ctor #f))))

; An instance expands to a constructor call for the corresponding interface.
; If the multiplicity is greater than 1, a vector of channels is created.
(define-syntax-parse-rule (instance _ (~optional ((~literal multiplicity) mult)) comp-name arg ...)
  #:with m (or (attribute mult) #'1)
  #:with inst-ctor-name (instance-ctor-name #'comp-name)
  (let ([ctor (λ (z) (inst-ctor-name arg ...))])
    (if (> m 1)
      (build-vector m ctor)
      (ctor #f))))

; A constant expands to a variable definition.
; TODO: define constants as fields in interfaces and components.
(define-syntax-parse-rule (constant name expr)
  (define name expr))

; An alias expands to a variable that receives the result of the accessor
; for the target port.
(define-syntax-parse-rule (alias name port-name intf-name)
  (define name (field-expr (name-expr port-name) name intf-name)))

; An assignment fills the target port's box with the signal
; from the right-hand side.
(define-syntax-parse-rule (assignment target expr)
  (set-box! target expr))

; A literal expression expands to its value.
(define-syntax-parse-rule (literal-expr value)
  value)

; A name expression expands to the corresponding variable name in the current scope.
(define-syntax-parse-rule (name-expr name)
  name)

; After semantic checking, a field expression contains the name
; of the interface or record type where the field is declared.
; A field expression expands to a field access in a struct instance.
(define-syntax-parse-rule (field-expr expr field-name type-name)
  #:with acc (accessor-name #'type-name #'field-name)
  (acc expr))

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
  [(_ i d)                                 #'(register    i     d)]
  [(_ i (when-clause r) d)                 #'(register/r  i r   d)]
  [(_ i d (when-clause e))                 #'(register/e  i   e d)]
  [(_ i (when-clause r) d (when-clause e)) #'(register/re i r e d)])

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
    (assignment (field-expr (name-expr i) y I0) (signal-expr (field-expr (name-expr i) x I0))))

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
    (assignment (field-expr (name-expr i) y I2)
                (signal-expr (field-expr (name-expr i) x I2))))

  (component C4
    (composite-port i (multiplicity 3) I2)
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 0)) y I2)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 0)) x I2)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 1)) y I2)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 1)) x I2)))
    (assignment (field-expr (indexed-expr (name-expr i) (literal-expr 2)) y I2)
                (signal-expr (field-expr (indexed-expr (name-expr i) (literal-expr 2)) x I2))))

  (interface I8
    (data-port x in (name-expr integer)))

  (component C5
    (composite-port i (multiplicity (literal-expr 3)) I8)
    (data-port y in (name-expr integer))
    (data-port z out (name-expr integer))
    (assignment (name-expr z)
                (lift-expr [y^ (signal-expr (name-expr y))]
                           (signal-expr (field-expr (indexed-expr (name-expr i) (name-expr y^)) x I8)))))

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
    (assignment (field-expr (name-expr c) x C8) (signal-expr (name-expr x)))
    (assignment (name-expr y) (signal-expr (field-expr (name-expr c) y C8))))

  (component C10
    (data-port x0 in #f)
    (data-port x1 in #f)
    (data-port y out #f)
    (instance c (multiplicity 2) C8 10)
    (assignment (field-expr (indexed-expr (name-expr c) 0) x C8) (signal-expr (name-expr x0)))
    (assignment (field-expr (indexed-expr (name-expr c) 1) x C8) (signal-expr (name-expr x1)))
    (assignment (name-expr y) (lift-expr [y0 (signal-expr (field-expr (indexed-expr (name-expr c) 0) y C8))]
                                         [y1 (signal-expr (field-expr (indexed-expr (name-expr c) 1) y C8))]
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

  (define (check-sig-equal? t e n)
    (check-equal? (signal-take t n) (signal-take e n)))

  (define .+ (signal-lift +))
  (define .* (signal-lift *))

  (test-case "Can construct a channel for an interface with simple ports"
    (define i (I0-make-channel 30))
    (check-pred box? (I0-x i))
    (check-pred box? (I0-y i)))

  (test-case "Can construct a channel for a component with simple ports"
    (define c (C0-make-instance 30))
    (check-pred box? (C0-x c))
    (check-pred box? (C0-y c)))

  (test-case "Can construct a channel for an interface with composite ports and no parameters"
    (define i3 (I3-make-channel))
    (check-pred box? (I3-z i3))
    (check-pred I2? (I3-i i3))
    (check-pred box? (I2-x (I3-i i3)))
    (check-pred box? (I2-y (I3-i i3)))
    (check-pred box? (I3-u i3))

    (define i4 (I4-make-channel))
    (check-pred box? (I4-v i4))
    (check-pred I3? (I4-j i4))
    (check-pred box? (I3-z (I4-j i4)))
    (check-pred box? (I2-x (I3-i (I4-j i4))))
    (check-pred box? (I2-y (I3-i (I4-j i4))))
    (check-pred box? (I3-u (I4-j i4)))
    (check-pred box? (I4-w i4)))

  (test-case "Can construct a channel for a component with composite ports and no parameters"
    (define c (C2-make-instance))
    (check-pred box? (C2-v c))
    (check-pred I3? (C2-j c))
    (check-pred box? (I3-z (C2-j c)))
    (check-pred box? (I2-x (I3-i (C2-j c))))
    (check-pred box? (I2-y (I3-i (C2-j c))))
    (check-pred box? (I3-u (C2-j c)))
    (check-pred box? (C2-w c)))

  (test-case "Can construct a channel for an interface with a vector port"
    (define j (I5-make-channel))
    (check-pred vector? (I5-i j))
    (check-eq? (vector-length (I5-i j)) 3)
    (for ([n (range 3)])
      (check-pred I2? (vector-ref (I5-i j) n))))

  (test-case "Can construct a channel for an interface with arguments"
    (define j (I6-make-channel 5))
    (check-pred vector? (I6-i j))
    (check-eq? (vector-length (I6-i j)) 5)
    (for ([n (range 5)])
      (check-pred I2? (vector-ref (I6-i j) n))))

  (test-case "Can construct a channel containing a composite port with arguments"
    (define k (I7-make-channel 3))
    (check-pred vector? (I6-i (I7-j k)))
    (check-eq? (vector-length (I6-i (I7-j k))) 3)
    (for ([n (range 3)])
      (check-pred I2? (vector-ref (I6-i (I7-j k)) n))))

  (test-case "Can assign a simple port to another simple port"
    (define c (C0-make-instance #f))
    (define x (signal 23))
    (port-set! (c C0-x) x)
    (check-sig-equal? (port-ref c C0-y) x 5))

  (test-case "Can access simple ports in a composite port"
    (define c (C3-make-instance))
    (define x (signal 23))
    (port-set! (c C3-i I2-x) x)
    (check-sig-equal? (port-ref c C3-i I2-y) x 5))

  (test-case "Can access simple ports in a vector composite port with static indices"
    (define c (C4-make-instance))
    (define x0 (signal 10))
    (define x1 (signal 20))
    (define x2 (signal 30))
    (port-set! (c C4-i 0 I2-x) x0)
    (port-set! (c C4-i 1 I2-x) x1)
    (port-set! (c C4-i 2 I2-x) x2)
    (check-sig-equal? (port-ref c C4-i 0 I2-x) x0 5)
    (check-sig-equal? (port-ref c C4-i 1 I2-x) x1 5)
    (check-sig-equal? (port-ref c C4-i 2 I2-x) x2 5))

  (test-case "Can access simple ports in a vector composite port with dynamic indices"
    (define c (C5-make-instance))
    (define x0 (signal 10))
    (define x1 (signal 20))
    (define x2 (signal 30))
    (define y (list->signal (list 0 1 2 1 0 2)))
    (port-set! (c C5-i 0 I8-x) x0)
    (port-set! (c C5-i 1 I8-x) x1)
    (port-set! (c C5-i 2 I8-x) x2)
    (port-set! (c C5-y)        y)
    (define z (list->signal (list 10 20 30 20 10 30)))
    (check-sig-equal? (port-ref c C5-z) z 5))

  (test-case "Can perform an operation between signals"
    (define c (C6-make-instance))
    (define x (list->signal (range 1  5  1)))
    (define y (list->signal (range 10 50 10)))
    (port-set! (c C6-x) x)
    (port-set! (c C6-y) y)
    (check-sig-equal? (port-ref c C6-z) (.+ x y) 5))

  (test-case "Can use local signals"
    (define c (C7-make-instance))
    (define x (list->signal (list 10 20 30 40 50)))
    (define y (signal 2))
    (define z (list->signal (list 1 2 3 4 5)))
    (define u (signal 3))
    (port-set! (c C7-x) x)
    (port-set! (c C7-y) y)
    (port-set! (c C7-z) z)
    (port-set! (c C7-u) u)
    (check-sig-equal? (port-ref c C7-v) (.+ (.* x y) (.* z u)) 5))

  (test-case "Can instantiate a component"
    (define c (C9-make-instance))
    (define x (list->signal (list 10 20 30 40 50)))
    (port-set! (c C9-x) x)
    (check-sig-equal? (port-ref c C9-y) (.* x (signal 10)) 5))

  (test-case "Can instantiate a multiple component"
    (define c (C10-make-instance))
    (define x0 (list->signal (list 10 20 30 40 50)))
    (define x1 (list->signal (list 1 2 3 4 5)))
    (port-set! (c C10-x0) x0)
    (port-set! (c C10-x1) x1)
    (check-sig-equal? (port-ref c C10-y) (.* (.+ x0 x1) (signal 10)) 5))

  (test-case "Can register a signal"
    (define c (C11-make-instance))
    (define x (list->signal (list 10 20  30 40 50)))
    (port-set! (c C11-x) x)
    (check-sig-equal? (port-ref c C11-y) (register 0 x) 6))

  (test-case "Can register a signal with reset"
    (define c (C12-make-instance))
    (define x (list->signal (list #f #f  #f #t #f)))
    (define y (list->signal (list 10 20  30 40 50)))
    (port-set! (c C12-x) x)
    (port-set! (c C12-y) y)
    (check-sig-equal? (port-ref c C12-z) (register/r 0 x y) 6))

  (test-case "Can register a signal with enable"
    (define c (C13-make-instance))
    (define x (list->signal (list #f #t  #f #t #f)))
    (define y (list->signal (list 10 20  30 40 50)))
    (port-set! (c C13-x) x)
    (port-set! (c C13-y) y)
    (check-sig-equal? (port-ref c C13-z) (register/e 0 x y) 6))

  (test-case "Can register a signal with reset and enable"
    (define c (C14-make-instance))
    (define x (list->signal (list #f #t  #f #t #f)))
    (define y (list->signal (list #f #f  #t #f #f)))
    (define z (list->signal (list 10 20  30 40 50)))
    (port-set! (c C14-x) x)
    (port-set! (c C14-y) y)
    (port-set! (c C14-z) z)
    (check-sig-equal? (port-ref c C14-u) (register/re 0 x y z) 6)))
