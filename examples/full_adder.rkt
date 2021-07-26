#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "full_adder.hdrml")

(define inst (full_adder-make))

(port-set! (inst a)  (signal 0 0 0 0 1 1 1 1))
(port-set! (inst b)  (signal 0 0 1 1 0 0 1 1))
(port-set! (inst ci) (signal 0 1 0 1 0 1 0 1))

(define duration 8)

(for ([(name sig) (in-dict (signal-table inst))])
  (printf "~a = ~a\n" name (signal-take sig duration)))
