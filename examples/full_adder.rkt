#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "full_adder.hdrml")

(define inst (full_adder-make))

(slot-set! (inst a)  (signal 0 0 0 0 1 1 1 1))
(slot-set! (inst b)  (signal 0 0 1 1 0 0 1 1))
(slot-set! (inst ci) (signal 0 1 0 1 0 1 0 1))

(define duration 8)

(print-slot-table (slot-table inst) duration)
