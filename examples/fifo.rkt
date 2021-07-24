#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.hdrml")

(define f (fifo-make-instance #f 0 1))

(port-set! (f fifo-c producer-valid) (signal 0  1  1  1))
(port-set! (f fifo-c producer-data)  (signal 10 10 20 30))
(port-set! (f fifo-p producer-ready) (signal 0  0  0  0))

(define c-valid (port-ref f fifo-c producer-valid))
(define c-ready (port-ref f fifo-c producer-ready))
(define c-data  (port-ref f fifo-c producer-data))
(define p-valid (port-ref f fifo-p producer-valid))
(define p-ready (port-ref f fifo-p producer-ready))
(define p-data  (port-ref f fifo-p producer-data))

(vcd
  (waveforms
    (c-valid 1)
    (c-ready 1)
    (c-data  8)
    (p-valid 1)
    (p-ready 1)
    (p-data  8))
  10 "10 ns"
  (open-output-file "fifo.vcd" #:exists 'replace))
