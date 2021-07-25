#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.hdrml")

(define f (fifo-make-instance #f 0 3))

(port-set! (f fifo-c producer-valid) (signal 0  1  1  1  1  1 1 0))
(port-set! (f fifo-c producer-data)  (signal 10 10 20 30 40))
(port-set! (f fifo-p producer-ready) (signal 0  0  0  0  0  0 1 0 1 0 1 0 1 0))

(define c-valid (port-ref f fifo-c producer-valid))
(define c-ready (port-ref f fifo-c producer-ready))
(define c-data  (port-ref f fifo-c producer-data))

(define m-p-valid (port-ref f fifo-m mailbox-p producer-valid))
(define m-p-ready (port-ref f fifo-m mailbox-p producer-ready))
(define m-p-data  (port-ref f fifo-m mailbox-p producer-data))

(define p-valid (port-ref f fifo-p producer-valid))
(define p-ready (port-ref f fifo-p producer-ready))
(define p-data  (port-ref f fifo-p producer-data))

(vcd
  (waveforms
    (c-valid 1)
    (c-ready 1)
    (c-data  8)
    (m-p-valid 1)
    (m-p-ready 1)
    (m-p-data  8)
    (p-valid 1)
    (p-ready 1)
    (p-data  8))
  15 "10 ns"
  (open-output-file "fifo3.vcd" #:exists 'replace))
