#lang racket

(require
  "signal.rkt"
  syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (== a b)
  (if (equal? a b) 1 0))

(define-syntax-parse-rule (/= a b)
  (if (equal? a b) 0 1))

(define-syntax-parse-rule (hydromel-if a b c)
  (if (zero? a) c b))
