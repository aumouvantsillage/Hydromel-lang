#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "half_adder.hdrml")

(define inst (half_adder-make-instance))

(port-set! (inst half_adder-a) (signal 0 0 1 1))
(port-set! (inst half_adder-b) (signal 0 1 0 1))

(define s (signal-take (port-ref inst half_adder-s) 4))
(define c (signal-take (port-ref inst half_adder-c) 4))

(printf "s = ~a\n" s)
(printf "c = ~a\n" c)
