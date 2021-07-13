#lang racket

(require
  syntax/parse/define
  racket/stxparam
  "signal.rkt"
  (for-syntax
    (only-in racket empty identity in-syntax match)
    racket/syntax
    syntax/parse/define
    (prefix-in stx/ "syntax.rkt")))

(provide
  module
  interface
  component
  constant
  local-signal
  assignment
  literal-expr
  alias
  name-expr
  field-expr
  indexed-expr
  call-expr
  register-expr
  signal-expr
  static-expr
  lift-expr)

(begin-for-syntax
  (define (channel-ctor-name name)
    (format-id name "make-channel-~a" name))

  (define (instance-ctor-name name)
    (format-id name "make-instance-~a" name))

  (define (accessor-name sname fname)
    (format-id fname "~a-~a" sname fname))

  ; This macro can be used to implement map, filter, or a combination of both,
  ; on a list syntax object.
  ;
  ; It defines a function that parses the elements of a list syntax object.
  ; The body is composed of parse options and clauses supported by syntax-parse.
  ; Elements that do not match any syntax pattern are filtered out.
  (define-simple-macro (define/map-syntax name body ...)
    (define (name lst)
      (filter identity
        (for/list ([i (in-list (or lst empty))])
          (syntax-parse i
            body ... [_ #f])))))

  ; Return the list of ports and signals in the given syntax object.
  (define/map-syntax design-unit-field-syntaxes
    [:stx/data-port      this-syntax]
    [:stx/composite-port this-syntax]
    [:stx/local-signal   this-syntax]
    [:stx/instance       this-syntax])

  ; Return the list of port and signal names in the given syntax object.
  (define/map-syntax design-unit-field-names
    [:stx/data-port      #'name]
    [:stx/composite-port #'name]
    [:stx/local-signal   #'name]
    [:stx/instance       #'name])

  (define/map-syntax design-unit-statements
    [:stx/constant     this-syntax]
    [:stx/local-signal this-syntax]
    [:stx/alias        this-syntax]
    [:stx/assignment   this-syntax])

  (define/map-syntax design-unit-aliases
    [:stx/alias this-syntax])

  ; Return the list of parameter names in the given syntax object.
  (define/map-syntax design-unit-parameter-names
    [:stx/parameter #'name]))

; Generate a module.
(define-simple-macro (module body ...)
  (begin body ...))

; Generate a provide clause only in a module context.
(define-syntax-parser provide*
  [(provide* spec ...)
   (match (syntax-local-context)
     ['module       #'(provide spec ...)]
     ['module-begin #'(provide spec ...)]
     [_             #'(begin)])])

(define-syntax (parameter stx)
  (raise-syntax-error #f "should be used inside an interface or component" stx))

; Generate a struct type and a constructor function from an interface.
(define-simple-macro (interface name body ...)
  #:with ctor-name        (channel-ctor-name #'name)
  #:with (param-name ...) (design-unit-parameter-names (attribute body))
  #:with (field-name ...) (design-unit-field-names     (attribute body))
  #:with (field-stx ...)  (design-unit-field-syntaxes  (attribute body))
  #:with (alias-stx ...)  (design-unit-aliases         (attribute body))
  (begin
    (provide* (struct-out name) ctor-name)
    (struct name (field-name ...) #:transparent)
    (define (ctor-name param-name ...)
      (name field-stx ...))
    (alias-accessor name alias-stx) ...))

(define-simple-macro (alias-accessor parent-intf-name (alias alias-name port-name alias-intf-name))
  #:with port-acc-name (accessor-name #'parent-intf-name #'port-name)
  #:with orig-acc-name (accessor-name #'alias-intf-name  #'alias-name)
  #:with acc-name      (accessor-name #'parent-intf-name #'alias-name)
  (begin
    (provide* acc-name)
    (define (acc-name x)
      (orig-acc-name (port-acc-name x)))))

; Inside a component, this parameter becomes true when expanding statements.
; This is useful for constructs that are expanded twice, as fields and as statements.
(define-syntax-parameter as-statement #f)

; From a component, generate the same output as for an interface,
; and a function with the body of the component.
(define-simple-macro (component name body ...)
   #:with inst-ctor-name   (instance-ctor-name #'name)
   #:with chan-ctor-name   (channel-ctor-name  #'name)
   #:with (param-name ...) (design-unit-parameter-names (attribute body))
   #:with (field-name ...) (design-unit-field-names     (attribute body))
   #:with (stmt ...)       (design-unit-statements      (attribute body))
   #:with (field-acc-name ...) (for/list ([fname (in-list (attribute field-name))])
                                 (accessor-name #'name fname))
   (begin
     (interface name body ...)
     (provide* inst-ctor-name)
     (define (inst-ctor-name param-name ...)
       (define self (chan-ctor-name param-name ...))
       (define field-name (field-acc-name self)) ...
       (syntax-parameterize ([as-statement #t])
         stmt ...)
       self)))

; Data port initialization in a channel constructor.
(define-simple-macro (data-port _ ...)
  (box #f))

(define-syntax-parser local-signal
  [(local-signal name expr)
   (if (syntax-parameter-value #'as-statement)
     ; Local signal assignment in a component body.
     #'(set-box! name expr)
     ; Local signal initialization in a channel constructor.
     #'(box #f))])

(define-syntax (multiplicity stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

(define-syntax (splice stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

(define-syntax (flip stx)
  (raise-syntax-error #f "should be used inside a composite port declaration" stx))

; Composite port initialization in a channel constructor.
(define-simple-macro (composite-port _ (~optional (multiplicity mult)) (~or (~literal splice) (~literal flip)) ... intf-name arg ...)
  #:with m (or (attribute mult) #'1)
  #:with chan-ctor-name (channel-ctor-name #'intf-name)
  (let ([ctor (λ (z) (chan-ctor-name arg ...))])
    (if (> m 1)
      (build-vector m ctor)
      (ctor #f))))

; Instance initialization in a channel constructor.
(define-simple-macro (instance _ (~optional ((~literal multiplicity) mult)) comp-name arg ...)
  #:with m (or (attribute mult) #'1)
  #:with inst-ctor-name (instance-ctor-name #'comp-name)
  (let ([ctor (λ (z) (inst-ctor-name arg ...))])
    (if (> m 1)
      (build-vector m ctor)
      (ctor #f))))

; TODO: constant as field
(define-simple-macro (constant name expr)
  (define name expr))

(define-simple-macro (alias name port-name intf-name)
  (define name (field-expr (name-expr port-name) name intf-name)))

; An assignment fills the target port's box with the signal
; from the right-hand side.
(define-simple-macro (assignment target expr)
  (set-box! target expr))

; Expand a literal expression to its value.
(define-simple-macro (literal-expr value)
  value)

; A name expression refers to a variable in the current scope.
(define-simple-macro (name-expr name)
  name)

; After type checking, a field expression contains the name
; of the interface or record type where the field is declared.
; A field expression expands to a field access in a struct instance.
(define-simple-macro (field-expr expr field-name type-name)
  #:with acc (accessor-name #'type-name #'field-name)
  (acc expr))

; An indexed expression expands to a chain of vector accesses.
(define-syntax-parser indexed-expr
  [(indexed-expr expr index ... last)
   #'(vector-ref (indexed-expr expr index ...) last)]
  [(indexed-expr expr)
   #'expr])

(define-simple-macro (call-expr fn-name arg ...)
  (fn-name arg ...))

(define-syntax (when-clause stx)
  (raise-syntax-error #f "should be used inside a register expression" stx))

(define-syntax-parser register-expr
  #:literals [when-clause]
  [(register-expr i d)                                 #'(register    i     d)]
  [(register-expr i (when-clause r) d)                 #'(register/r  i r   d)]
  [(register-expr i d (when-clause e))                 #'(register/e  i   e d)]
  [(register-expr i (when-clause r) d (when-clause e)) #'(register/re i r e d)])

; A signal expression is a wrapper element added by the typechecker
; to identify an expression that refers to a port or local signal
; for reading.
(define-simple-macro (signal-expr expr)
  (signal-defer (unbox expr)))

(define-simple-macro (static-expr expr)
  (signal expr))

; A lift expression is a wrapper element added by the typechecker
; when an expression depends on some signal values.
; name ... is a list of signal names that are needed to compute expr.
(define-syntax-parser lift-expr
  #:literals [signal-expr]
  ; Lift a signal expression. Since the expression returns a signal,
  ; we must lift signal-first to avoid created a signal of signals.
  ; This is typically used when indexed-expr contains signals as indices.
  [(lift-expr binding ...+ (signal-expr expr))
   #'(lift-expr binding ... (signal-first (signal-expr expr)))]
  ; Lift any expression that computes values from values.
  ; expr must not contain elements of type signal-expr.
  [(lift-expr (name sexpr) ...+ expr)
   #'(for/signal ([name sexpr] ...) expr)])


(module+ test
  (require rackunit
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

  (define-syntax-parser port-ref*
    [(port-ref* x)                    #'x]
    [(port-ref* x f:identifier i ...) #'(port-ref* (f x) i ...)]
    [(port-ref* x n:number i ...)     #'(port-ref* (vector-ref x n) i ...)])

  (define-simple-macro (port-ref path ...)
    (unbox (port-ref* path ...)))

  (define-simple-macro (port-set! (path ...) value)
    (set-box! (port-ref* path ...) value))

  (define .+ (signal-lift +))
  (define .* (signal-lift *))

  (test-case "Interface with data ports is mapped to channel struct"
    (define i (I0 10 20))
    (check-eq? (I0-x i) 10)
    (check-eq? (I0-y i) 20))

  (test-case "Component with data ports is mapped to channel struct"
    (define c (C0 10 20))
    (check-eq? (C0-x c) 10)
    (check-eq? (C0-y c) 20))

  (test-case "Interface with composite ports is mapped to channel struct"
    (define j (I1 (I0 10 20) 30))
    (check-eq? (I0-x (I1-i j)) 10)
    (check-eq? (I0-y (I1-i j)) 20)
    (check-eq? (I1-z j) 30))

  (test-case "Component with composite ports is mapped to channel struct"
    (define c (C1 (I0 10 20) 30))
    (check-eq? (I0-x (C1-i c)) 10)
    (check-eq? (I0-y (C1-i c)) 20)
    (check-eq? (C1-z c) 30))

  (test-case "Can construct a channel for an interface with simple ports"
    (define i (make-channel-I0 30))
    (check-pred box? (I0-x i))
    (check-pred box? (I0-y i)))

  (test-case "Can construct a channel for a component with simple ports"
    (define c (make-instance-C0 30))
    (check-pred box? (C0-x c))
    (check-pred box? (C0-y c)))

  (test-case "Can construct a channel for an interface with composite ports and no parameters"
    (define i3 (make-channel-I3))
    (check-pred box? (I3-z i3))
    (check-pred I2? (I3-i i3))
    (check-pred box? (I2-x (I3-i i3)))
    (check-pred box? (I2-y (I3-i i3)))
    (check-pred box? (I3-u i3))

    (define i4 (make-channel-I4))
    (check-pred box? (I4-v i4))
    (check-pred I3? (I4-j i4))
    (check-pred box? (I3-z (I4-j i4)))
    (check-pred box? (I2-x (I3-i (I4-j i4))))
    (check-pred box? (I2-y (I3-i (I4-j i4))))
    (check-pred box? (I3-u (I4-j i4)))
    (check-pred box? (I4-w i4)))

  (test-case "Can construct a channel for a component with composite ports and no parameters"
    (define c (make-instance-C2))
    (check-pred box? (C2-v c))
    (check-pred I3? (C2-j c))
    (check-pred box? (I3-z (C2-j c)))
    (check-pred box? (I2-x (I3-i (C2-j c))))
    (check-pred box? (I2-y (I3-i (C2-j c))))
    (check-pred box? (I3-u (C2-j c)))
    (check-pred box? (C2-w c)))

  (test-case "Can construct a channel for an interface with a vector port"
    (define j (make-channel-I5))
    (check-pred vector? (I5-i j))
    (check-eq? (vector-length (I5-i j)) 3)
    (for ([n (range 3)])
      (check-pred I2? (vector-ref (I5-i j) n))))

  (test-case "Can construct a channel for an interface with arguments"
    (define j (make-channel-I6 5))
    (check-pred vector? (I6-i j))
    (check-eq? (vector-length (I6-i j)) 5)
    (for ([n (range 5)])
      (check-pred I2? (vector-ref (I6-i j) n))))

  (test-case "Can construct a channel containing a composite port with arguments"
    (define k (make-channel-I7 3))
    (check-pred vector? (I6-i (I7-j k)))
    (check-eq? (vector-length (I6-i (I7-j k))) 3)
    (for ([n (range 3)])
      (check-pred I2? (vector-ref (I6-i (I7-j k)) n))))

  (test-case "Can assign a simple port to another simple port"
    (define c (make-instance-C0 #f))
    (define x (signal 23))
    (port-set! (c C0-x) x)
    (check-sig-equal? (port-ref c C0-y) x 5))

  (test-case "Can access simple ports in a composite port"
    (define c (make-instance-C3))
    (define x (signal 23))
    (port-set! (c C3-i I2-x) x)
    (check-sig-equal? (port-ref c C3-i I2-y) x 5))

  (test-case "Can access simple ports in a vector composite port with static indices"
    (define c (make-instance-C4))
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
    (define c (make-instance-C5))
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
    (define c (make-instance-C6))
    (define x (list->signal (range 1  5  1)))
    (define y (list->signal (range 10 50 10)))
    (port-set! (c C6-x) x)
    (port-set! (c C6-y) y)
    (check-sig-equal? (port-ref c C6-z) (.+ x y) 5))

  (test-case "Can use local signals"
    (define c (make-instance-C7))
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
    (define c (make-instance-C9))
    (define x (list->signal (list 10 20 30 40 50)))
    (port-set! (c C9-x) x)
    (check-sig-equal? (port-ref c C9-y) (.* x (signal 10)) 5))

  (test-case "Can instantiate a multiple component"
    (define c (make-instance-C10))
    (define x0 (list->signal (list 10 20 30 40 50)))
    (define x1 (list->signal (list 1 2 3 4 5)))
    (port-set! (c C10-x0) x0)
    (port-set! (c C10-x1) x1)
    (check-sig-equal? (port-ref c C10-y) (.* (.+ x0 x1) (signal 10)) 5))

  (test-case "Can register a signal"
    (define c (make-instance-C11))
    (define x (list->signal (list 10 20  30 40 50)))
    (port-set! (c C11-x) x)
    (check-sig-equal? (port-ref c C11-y) (register 0 x) 6))

  (test-case "Can register a signal with reset"
    (define c (make-instance-C12))
    (define x (list->signal (list #f #f  #f #t #f)))
    (define y (list->signal (list 10 20  30 40 50)))
    (port-set! (c C12-x) x)
    (port-set! (c C12-y) y)
    (check-sig-equal? (port-ref c C12-z) (register/r 0 x y) 6))

  (test-case "Can register a signal with enable"
    (define c (make-instance-C13))
    (define x (list->signal (list #f #t  #f #t #f)))
    (define y (list->signal (list 10 20  30 40 50)))
    (port-set! (c C13-x) x)
    (port-set! (c C13-y) y)
    (check-sig-equal? (port-ref c C13-z) (register/e 0 x y) 6))

  (test-case "Can register a signal with reset and enable"
    (define c (make-instance-C14))
    (define x (list->signal (list #f #t  #f #t #f)))
    (define y (list->signal (list #f #f  #t #f #f)))
    (define z (list->signal (list 10 20  30 40 50)))
    (port-set! (c C14-x) x)
    (port-set! (c C14-y) y)
    (port-set! (c C14-z) z)
    (check-sig-equal? (port-ref c C14-u) (register/re 0 x y z) 6)))
