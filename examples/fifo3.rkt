#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.hdrml")

(define inst (fifo_rec_b-make (unsigned 8) 0 3))

(port-set! (inst c valid) (signal 0  1  1  1  1  1 1 0))
(port-set! (inst c data)  (signal 10 10 20 30 40))
(port-set! (inst p ready) (signal 0  0  0  0  0  0 1 0 1 0 1 0 1 0))

(define duration 15)

(define tbl (signal-table inst))

(for ([(name slt) (in-dict tbl)])
  (printf "~a : ~a = ~a\n" name (slot-type slt) (signal-take (slot-signal slt) duration)))

(vcd tbl duration "10 ns"
  (open-output-file "fifo3.vcd" #:exists 'replace))
