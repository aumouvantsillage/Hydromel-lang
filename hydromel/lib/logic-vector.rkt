#lang racket

(require
  "logic.rkt"
  "signal.rkt"
  syntax/parse/define
  (prefix-in t/ "types.rkt"))

(provide
  (struct-out logic-vector)
  make-logic-vector
  logic-signal
  logic-signal-take
  logic-vector-0
  logic-vector-1
  logic-vector-true?
  logic-vector-not logic-vector-not-signature
  logic-vector-and logic-vector-and-signature
  logic-vector-or  logic-vector-or-signature
  logic-vector-xor logic-vector-xor-signature
  logic-vector-==  logic-vector-==-signature
  logic-vector-/=  logic-vector-/=-signature
  logic-vector--   logic-vector---signature
  logic-vector-+   logic-vector-+-signature
  logic-vector-*   logic-vector-*-signature
  logic-vector->   logic-vector->-signature
  logic-vector-unsigned-width)
  
(struct logic-vector (value width signed?) #:transparent)

(define (make-logic-vector v)
  (if (< v 0)
    (let ([w (min-signed-width v)])
      (logic-vector (signed v w) w #t))
    (let ([w (min-unsigned-width v)])
      (logic-vector (unsigned v w) w #f))))

(define logic-vector-0 (make-logic-vector 0))
(define logic-vector-1 (make-logic-vector 1))

(define-syntax-parse-rule (logic-signal v ...)
  (signal (make-logic-vector v) ...))

(define (logic-signal-take s n)
  (map logic-vector-value (signal-take s n)))

(define (make-logic-vector* v w s)
  (logic-vector
    ((if s signed unsigned) v w)
    w s))

(define (logic-vector-true? a)
  (not (zero? (logic-vector-value a))))

(define (logic-vector-not a)
  (match-define (logic-vector v w s) a)
  (make-logic-vector* (bitwise-not v) w s))

(define (logic-vector-not-signature ta)
  ta)

(define (logic-vector-and a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (make-logic-vector* (bitwise-and va vb)
                      (max         na nb)
                      (or          sa sb)))

(define (logic-vector-max a b)
  (max (logic-vector-value a) (logic-vector-value b)))

(define (logic-vector-and-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (logic-vector-max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (logic-vector-max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (logic-vector-max na nb))]
    [_ 'invalid]))

(define (logic-vector-or a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (make-logic-vector* (bitwise-ior va vb)
                      (max         na nb)
                      (or          sa sb)))

(define (logic-vector-or-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (logic-vector-max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (logic-vector-max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (logic-vector-max na nb))]
    [_ 'invalid]))

(define (logic-vector-xor a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (make-logic-vector* (bitwise-xor va vb)
                      (max         na nb)
                      (or          sa sb)))

(define (logic-vector-xor-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (logic-vector-max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (logic-vector-max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (logic-vector-max na nb))]
    [_ 'invalid]))

(define (logic-vector-== a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (if (= va vb)
    logic-vector-1
    logic-vector-0))

(define (logic-vector-==-signature ta tb)
  (t/unsigned 1))

(define (logic-vector-/= a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (if (= va vb)
    logic-vector-0
    logic-vector-1))

(define (logic-vector-/=-signature ta tb)
  (t/unsigned 1))

(define (logic-vector-> a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (if (> va vb)
    logic-vector-1
    logic-vector-0))

(define (logic-vector->-signature ta tb)
  (t/unsigned 1))

(define (logic-vector-- a [b #f])
  (match-define (logic-vector va na sa) a)
  (if b
    (match-let ([(logic-vector vb nb sb) b])
      (make-logic-vector* (- va vb)
                          (add1 (max na nb))
                          #t))
    (make-logic-vector* (- va)
                        (if sa na (add1 na))
                        #t)))

(define (logic-vector---signature ta [tb #f])
  (match (cons ta tb)
    [(cons (t/signed   na) #f)             ta]
    [(cons (t/unsigned na) #f)             (t/signed (add1 (logic-vector-value na)))]
    [(cons (t/integer  na) (t/integer nb)) (t/signed (add1 (logic-vector-max na nb)))]
    [_ 'invalid]))

(define (logic-vector-+ a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (make-logic-vector* (+ va vb)
                      (add1 (max na nb))
                      (or sa sb)))

(define (logic-vector-+-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (logic-vector-max na nb)))]
    [(cons (t/signed   na) (t/integer nb)) (t/signed    (add1 (logic-vector-max na nb)))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (add1 (logic-vector-max na nb)))]
    [_ 'invalid]))

; TODO check result size
(define (logic-vector-* a b)
  (match-define (logic-vector va na sa) a)
  (match-define (logic-vector vb nb sb) b)
  (make-logic-vector* (* va vb)
                      (+ na nb)
                      (or sa sb)))

(define (logic-vector-*-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (+ na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (+ na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (+ na nb))]
    [_ 'invalid]))


(define (logic-vector-unsigned-width n)
  (make-logic-vector (logic-vector-width n)))
