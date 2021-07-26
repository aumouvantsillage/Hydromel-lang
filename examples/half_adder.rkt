#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "half_adder.hdrml")

(define inst (half_adder-make))

(port-set! (inst a) (signal 0 0 1 1))
(port-set! (inst b) (signal 0 1 0 1))

(define duration 4)

(for ([(name sig) (in-dict (signal-table inst))])
  (printf "~a = ~a\n" name (signal-take sig duration)))
