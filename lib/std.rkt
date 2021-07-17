#lang racket

(require
  "signal.rkt"
  syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (== a b)
  (if (equal? a b) 1 0))

(define-syntax-parse-rule (/= a b)
  (if (equal? a b) 0 1))

(define-signal (signal-to-bool b)
  (cond [(number? b)  (not (zero? b))]
        [(boolean? b) b]
        [else         (error "Value cannot be coerced to boolean" b)]))

(define-syntax-parse-rule (hydromel-if a b c)
  (if (zero? a) c b))
