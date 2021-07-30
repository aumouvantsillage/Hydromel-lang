#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/logic-vector
  hydromel/lib/vcd
  "fifo.hdrml")

(define inst (fifo1-make #f (make-logic-vector 0)))

(port-set! (inst c valid) (logic-signal 0  1  0  1  1  1  0  0  0))
(port-set! (inst c data)  (logic-signal 10 10 10 20 30 30 30 30 30))
(port-set! (inst p ready) (logic-signal 1  1  0  0  0  1  0  1  0))

(define duration 10)

(define sigs (signal-table inst))

(for ([(name sig) (in-dict sigs)])
  (printf "~a = ~a\n" name (logic-signal-take sig duration)))

(vcd sigs duration "10 ns"
  (open-output-file "fifo1.vcd" #:exists 'replace))
