#lang racket

(require
  "signal.rkt"
  "slot.rkt"
  (prefix-in t/ "types.rkt")
  (only-in "logic.rkt"
    min-unsigned-width min-signed-width
    min-signed-value   max-signed-value
    min-unsigned-value max-unsigned-value
    unsigned-slice     unsigned-concat)
  syntax/parse/define
  (for-syntax
    (prefix-in meta/ "meta.rkt")))

(provide
  int-to-bool    int-to-bool-impl int-to-bool-impl-signature
  kw-if          kw-if-impl       kw-if-impl-signature
  kw-not                          bitwise-not-signature
  kw-and                          bitwise-and-signature
  kw-or                           bitwise-ior-signature
  kw-xor                          bitwise-xor-signature
  kw-==          kw-==-impl       kw-==-impl-signature
  kw-/=          kw-/=-impl       kw-/=-impl-signature
  kw->           kw->-impl        kw->-impl-signature
  kw-+                            +-signature
  kw--                            --signature
  kw-*                            *-signature
  kw-/                            quotient-signature
  kw-range       kw-range-impl    kw-range-impl-signature
  kw-slice                        unsigned-slice-signature
  kw-concat      kw-concat-impl   kw-concat-impl-signature
  (all-from-out  "logic.rkt")
  signed_width                    min-signed-width-signature
  unsigned_width                  min-unsigned-width-signature
  cast           cast-impl        cast-impl-signature)

; Convert an integer to a boolean.
; This function is used in generated conditional statements.
; It is not available from Hydromel source code.
(define-syntax int-to-bool (meta/builtin-function #'int-to-bool-impl))

(define (int-to-bool-impl a)
  (not (zero? a)))

(define (int-to-bool-impl-signature t)
  (t/boolean))

; The Hydromel if statement is expanded to a call-expr
; to kw-if as if it were a function.
(define-syntax kw-if (meta/builtin-function #'kw-if-impl))

(define-syntax-parse-rule (kw-if-impl (~seq c t) ... e)
  (cond [(int-to-bool-impl c) t]
        ...
        [else e]))

(define (kw-if-impl-signature tc . ts)
  (t/union ts))

; Returns the minimum width to encode a given number
; as an unsigned integer.
(define-syntax unsigned_width (meta/builtin-function #'min-unsigned-width))

(define (min-unsigned-width-signature t)
  (define t^ (t/actual-type t))
  (t/unsigned (t/abstract-integer-width t^)))

; Returns the minimum width to encode a given number
; as an signed integer.
(define-syntax signed_width (meta/builtin-function #'min-signed-width))

(define (min-signed-width-signature t)
  (define t^ (t/actual-type t))
  (t/unsigned (match t^
                [(t/signed   n) n]
                [(t/unsigned n) (add1 n)]
                [_ (error "Cannot compute data size.")])))

; Boolean operators are all bitwise.
(define-syntax kw-not (meta/builtin-function #'bitwise-not))
(define-syntax kw-and (meta/builtin-function #'bitwise-and))
(define-syntax kw-or  (meta/builtin-function #'bitwise-ior))
(define-syntax kw-xor (meta/builtin-function #'bitwise-xor))

(define (bitwise-signature ta tb)
  (define ta^ (t/actual-type ta))
  (define tb^ (t/actual-type tb))
  (match (cons ta^ tb^)
    [(cons (t/unsigned na)          (t/unsigned nb))          (t/unsigned (max na nb))]
    [(cons (t/signed   na)          (t/abstract-integer  nb)) (t/signed   (max na nb))]
    [(cons (t/abstract-integer  na) (t/signed   nb))          (t/signed   (max na nb))]
    [_ (error "Bitwise operation expects integer operands.")]))

(define (bitwise-not-signature ta) ta)
(define bitwise-and-signature bitwise-signature)
(define bitwise-ior-signature bitwise-signature)
(define bitwise-xor-signature bitwise-signature)

; Comparison operations return integers 0 and 1.
(define-syntax kw-== (meta/builtin-function #'kw-==-impl))
(define-syntax kw-/= (meta/builtin-function #'kw-/=-impl))
(define-syntax kw->  (meta/builtin-function #'kw->-impl))

(define (kw-==-impl a b)
  (if (= a b) 1 0))

(define (kw-/=-impl a b)
  (if (= a b) 0 1))

(define (kw->-impl a b)
  (if (> a b) 1 0))

(define (comparison-signature ta tb)
  (t/unsigned 1))

(define kw-==-impl-signature comparison-signature)
(define kw-/=-impl-signature comparison-signature)
(define kw->-impl-signature  comparison-signature)

; Use the built-in arithmetic operators.
(define-syntax kw-+ (meta/builtin-function #'+))
(define-syntax kw-- (meta/builtin-function #'-))
(define-syntax kw-* (meta/builtin-function #'*))
(define-syntax kw-/ (meta/builtin-function #'quotient))

(define (+-signature ta tb)
  (define ta^ (t/actual-type ta))
  (define tb^ (t/actual-type tb))
  (match (cons ta^ tb^)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (max na nb)))]
    [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (add1 (max (add1 na) nb)))]
    [(cons (t/signed   na) (t/unsigned nb)) (t/signed   (add1 (max na (add1 nb))))]
    [(cons (t/signed   na) (t/signed   nb)) (t/signed   (add1 (max na nb)))]
    [_ (error "Arithmetic operation expects integer operands." ta^ tb^)]))

(define (--signature ta [tb #f])
  (if tb
    (+-signature ta tb)
    (t/signed (add1 (t/abstract-integer-width (t/actual-type ta))))))

(define (*-signature ta tb)
  (define ta^ (t/actual-type ta))
  (define tb^ (t/actual-type tb))
  (match (cons ta^ tb^)
    [(cons (t/unsigned na)         (t/unsigned nb))          (t/unsigned (+ na nb))]
    [(cons (t/abstract-integer na) (t/signed   nb))          (t/signed   (+ na nb))]
    [(cons (t/signed na)           (t/abstract-integer  nb)) (t/signed   (+ na nb))]
    [_ (error "Arithmetic operation expects integer operands.")]))

(define (quotient-signature ta tb)
  ta)

; TODO descending ranges
(define-syntax kw-range (meta/builtin-function #'kw-range-impl))

(define (kw-range-impl a b)
  (range a (add1 b)))

(define (kw-range-impl-signature ta tb)
  (define ta^ (t/actual-type ta))
  (define tb^ (t/actual-type tb))
  (t/range
    (match (cons ta^ tb^)
      [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
      [(cons (t/unsigned na) (t/signed   nb)) (t/signed   (max (add1 na) nb))]
      [(cons (t/signed   na) (t/unsigned nb)) (t/signed   (max na (add1 nb)))]
      [(cons (t/signed   na) (t/signed   nb)) (t/signed   (max na nb))]
      [_ (error "Range expects integer boundaries.")])))

; The slicing operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the signature.
(define-syntax kw-slice (meta/builtin-function #'unsigned-slice))

(define (unsigned-slice-signature ta tb tc)
  (define left (match tb
                 [(t/static-data n _) n]
                 [(t/unsigned    n)   (max-unsigned-value n)]
                 [(t/signed      n)   (max-signed-value   n)]
                 [_                   (error "Invalid type for left slice index.")]))
  (define right (match tc
                 [(t/static-data n _) n]
                 [(t/unsigned    n)   (min-unsigned-value n)]
                 [(t/signed      n)   (min-signed-value   n)]
                 [_                   (error "Invalid type for right slice index.")]))
  (define width (max 0 (add1 (- left right))))
  (match (t/actual-type ta)
    [(t/unsigned _) (t/unsigned width)]
    [(t/signed   _) (t/signed   width)]
    [_ (error "Slice expects integer value.")]))

(define-syntax kw-concat (meta/builtin-function #'kw-concat-impl))

; The binary concatenetion operation defaults to the unsigned version.
; The signed case is handled automatically because the expander inserts
; a conversion to the type returned by the signature.
; Since this function needs to know the width of its arguments,
; their types are inserted by the checker.
(define-syntax-parse-rule (kw-concat-impl (~seq v t) ...)
  (unsigned-concat [v (sub1 (t/abstract-integer-width (t/actual-type t))) 0] ...))

(define (kw-concat-impl-signature . ts)
  (define ts^ (map t/actual-type ts))
  (define w (for/sum ([t (in-list ts^)]
                      [i (in-naturals)] #:when (odd? i))
              ; TODO assert that t contains an integer type
              (t/abstract-integer-width (t/type-supertype t))))
  (match (first ts^)
    [(t/signed _)   (t/signed w)]
    [(t/unsigned _) (t/unsigned w)]))

(define-syntax cast (meta/builtin-function #'cast-impl))

; cast does not actually convert the given value because
; a call to the conversion function is already inserted by the expander.
(define (cast-impl t e)
  e)

(define (cast-impl-signature ta tb)
  (t/type-supertype (t/actual-type ta)))
