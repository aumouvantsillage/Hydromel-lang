#lang racket

(require
  "logic.rkt"
  "signal.rkt"
  syntax/parse/define
  (prefix-in t/ "types.rkt"))

(provide
  logic-vector-not logic-vector-not-signature
  logic-vector-and logic-vector-and-signature
  logic-vector-or  logic-vector-or-signature
  logic-vector-xor logic-vector-xor-signature
  logic-vector-==  logic-vector-==-signature
  logic-vector-/=  logic-vector-/=-signature
  logic-vector--   logic-vector---signature
  logic-vector-+   logic-vector-+-signature
  logic-vector-*   logic-vector-*-signature
  logic-vector->   logic-vector->-signature)

(define logic-vector-not bitwise-not)

(define (logic-vector-not-signature ta)
  ta)

(define logic-vector-and bitwise-and)

(define (logic-vector-and-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (max na nb))]
    [_ 'invalid-and]))

(define logic-vector-or bitwise-ior)

(define (logic-vector-or-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (max na nb))]
    [_ 'invalid-or]))

(define logic-vector-xor bitwise-xor)

(define (logic-vector-xor-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (max na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (max na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (max na nb))]
    [_ 'invalid-xor]))

(define (logic-vector-== a b)
  (if (= a b) 1 0))

(define (logic-vector-==-signature ta tb)
  (t/unsigned 1))

(define (logic-vector-/= a b)
  (if (= a b) 0 1))

(define (logic-vector-/=-signature ta tb)
  (t/unsigned 1))

(define (logic-vector-> a b)
  (if (> a b) 1 0))

(define (logic-vector->-signature ta tb)
  (t/unsigned 1))

(define logic-vector-- -)

(define (logic-vector---signature ta [tb #f])
  (match (cons ta tb)
    [(cons (t/signed   na) #f)             ta]
    [(cons (t/unsigned na) #f)             (t/signed (add1 na))]
    [(cons (t/integer  na) (t/integer nb)) (t/signed (add1 (max na nb)))]
    [_ 'invalid-]))

(define logic-vector-+ +)

(define (logic-vector-+-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (add1 (max na nb)))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (add1 (max na nb)))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (add1 (max na nb)))]
    [_ 'invalid+]))

; TODO check result size
(define logic-vector-* *)

(define (logic-vector-*-signature ta tb)
  (match (cons ta tb)
    [(cons (t/unsigned na) (t/unsigned nb)) (t/unsigned (+ na nb))]
    [(cons (t/signed   na) (t/integer  nb)) (t/signed   (+ na nb))]
    [(cons (t/integer  na) (t/signed   nb)) (t/signed   (+ na nb))]
    [_
     (printf "~a ~a\n" ta tb)
     'invalid*]))
