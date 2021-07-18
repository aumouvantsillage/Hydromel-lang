#lang racket

(require
  "signal.rkt"
  syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (hydromel-== a b)
  (if (equal? a b) 1 0))

(define-syntax-parse-rule (hydromel-/= a b)
  (if (equal? a b) 0 1))

(define-syntax-parse-rule (hydromel-if c t e)
  (if (zero? c) e t))

(define hydromel-not bitwise-not)
(define hydromel-and bitwise-and)
(define hydromel-or  bitwise-ior)
(define hydromel-xor bitwise-xor)
