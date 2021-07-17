#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "counter.hdrml")

(define inst (counter-make-instance 3))

(port-set! (inst counter-clear) (signal 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
(port-set! (inst counter-up)    (signal 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0))
(port-set! (inst counter-down)  (signal 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0))

(define v (signal-take (port-ref inst counter-value)  19))
(define b (signal-take (port-ref inst counter-bottom) 19))
(define t (signal-take (port-ref inst counter-top)    19))

(printf "value  = ~a\n" v)
(printf "top    = ~a\n" t)
(printf "bottom = ~a\n" b)
