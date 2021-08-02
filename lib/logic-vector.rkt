#lang racket

(require
  "logic.rkt"
  "signal.rkt"
  syntax/parse/define)

(provide
  (struct-out logic-vector)
  make-logic-vector
  logic-signal
  logic-signal-take
  logic-vector-0
  logic-vector-1
  logic-vector-true?
  logic-vector-not
  logic-vector-and
  logic-vector-or
  logic-vector-xor
  logic-vector-==
  logic-vector-/=
  logic-vector--
  logic-vector-+
  logic-vector-*
  logic-vector->)

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

(define (logic-vector-and a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (make-logic-vector* (bitwise-and va vb)
                      (max         wa wb)
                      (or          sa sb)))

(define (logic-vector-or a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (make-logic-vector* (bitwise-ior va vb)
                      (max         wa wb)
                      (or          sa sb)))

(define (logic-vector-xor a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (make-logic-vector* (bitwise-xor va vb)
                      (max         wa wb)
                      (or          sa sb)))

(define (logic-vector-== a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (if (= va vb)
    logic-vector-1
    logic-vector-0))

(define (logic-vector-/= a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (if (= va vb)
    logic-vector-0
    logic-vector-1))

(define (logic-vector-> a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (if (> va vb)
    logic-vector-1
    logic-vector-0))

(define (logic-vector-- a [b #f])
  (match-define (logic-vector va wa sa) a)
  (if b
    (match-let ([(logic-vector vb wb sb) b])
      (make-logic-vector* (- va vb)
                          (add1 (max wa wb))
                          #t))
    (make-logic-vector* (- va)
                        (if sa wa (add1 wa))
                        #t)))

(define (logic-vector-+ a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (make-logic-vector* (+ va vb)
                      (add1 (max wa wb))
                      (or sa sb)))

; TODO check result size
(define (logic-vector-* a b)
  (match-define (logic-vector va wa sa) a)
  (match-define (logic-vector vb wb sb) b)
  (make-logic-vector* (* va vb)
                      (+ wa wb)
                      (or sa sb)))
