#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "full_adder.hdrml")

(define inst (full_adder-make-instance))

(port-set! (inst full_adder-a)  (signal 0 0 0 0 1 1 1 1))
(port-set! (inst full_adder-b)  (signal 0 0 1 1 0 0 1 1))
(port-set! (inst full_adder-ci) (signal 0 1 0 1 0 1 0 1))

(define s  (signal-take (port-ref inst full_adder-s)  8))
(define co (signal-take (port-ref inst full_adder-co) 8))

(printf "s  = ~a\n" s)
(printf "co = ~a\n" co)
