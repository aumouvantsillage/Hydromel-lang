#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/logic-vector
  "half_adder.hdrml")

(define inst (half_adder-make))

(port-set! (inst a) (logic-signal 0 0 1 1))
(port-set! (inst b) (logic-signal 0 1 0 1))

(define duration 4)

(for ([(name slt) (in-dict (signal-table inst))])
  (printf "~a = ~a\n" name (logic-signal-take (slot-signal slt) duration)))
