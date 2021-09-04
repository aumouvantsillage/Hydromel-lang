#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "half_adder.hdrml")

(define inst (half_adder-make))

(slot-set! (inst a) (signal 0 0 1 1))
(slot-set! (inst b) (signal 0 1 0 1))

(define duration 4)

(print-slot-table (slot-table inst) duration)
