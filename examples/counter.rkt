#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/logic-vector
  "counter.hdrml")

(define inst (counter-make (make-logic-vector 3)))

(port-set! (inst clear) (logic-signal 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
(port-set! (inst up)    (logic-signal 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0))
(port-set! (inst down)  (logic-signal 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0))

(define duration 19)

(for ([(name sig) (in-dict (signal-table inst))])
  (printf "~a = ~a\n" name (logic-signal-take sig duration)))
