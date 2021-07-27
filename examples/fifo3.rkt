#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.hdrml")

(define inst (fifo_rec_b-make #f 0 3))

(port-set! (inst c valid) (signal 0  1  1  1  1  1 1 0))
(port-set! (inst c data)  (signal 10 10 20 30 40))
(port-set! (inst p ready) (signal 0  0  0  0  0  0 1 0 1 0 1 0 1 0))

(define duration 15)

(define sigs (signal-table inst))

(for ([(name sig) (in-dict sigs)])
  (printf "~a = ~a\n" name (signal-take sig duration)))

(define wav-size 8) ; TODO set correct data size for each signal

(vcd
  (for/list ([(name sig) (in-dict sigs)])
    (waveform name wav-size sig))
  duration "10 ns"
  (open-output-file "fifo3.vcd" #:exists 'replace))
