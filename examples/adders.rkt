#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "adders.hdrml")

(define inst (ripple_carry_adder_rec-make 4))

(port-set! (inst a)  (signal 0 0 4 4 10 0 0 4 4 10))
(port-set! (inst b)  (signal 0 5 0 5 12 0 5 0 5 12))
(port-set! (inst ci) (signal 0 0 0 0  0 1 1 1 1  1))

(define duration 10)

(for ([(name slt) (in-dict (signal-table inst))])
  (printf "~a : ~v = ~a\n" name (slot-type slt) (signal-take (slot-data slt) duration)))
