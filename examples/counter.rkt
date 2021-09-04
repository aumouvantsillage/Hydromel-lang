#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "counter.hdrml")

(define inst (counter-make 3))

(slot-set! (inst clear) (signal 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
(slot-set! (inst up)    (signal 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0))
(slot-set! (inst down)  (signal 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0))

(define duration 19)

(print-slot-table (slot-table inst) duration)
