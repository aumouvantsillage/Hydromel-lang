#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.hdrml")

(define f (mailbox-make-instance #f 0))

(port-set! (f mailbox-c producer-valid) (signal 0  1  0  1  1  1  0  0  0))
(port-set! (f mailbox-c producer-data)  (signal 10 10 10 20 30 30 30 30 30))
(port-set! (f mailbox-p producer-ready) (signal 1  1  0  0  0  1  0  1  0))

(define c-valid (port-ref f mailbox-c producer-valid))
(define c-ready (port-ref f mailbox-c producer-ready))
(define c-data  (port-ref f mailbox-c producer-data))
(define m-full  (port-ref f mailbox-full))
(define m-write (port-ref f mailbox-write))
(define p-valid (port-ref f mailbox-p producer-valid))
(define p-ready (port-ref f mailbox-p producer-ready))
(define p-data  (port-ref f mailbox-p producer-data))

(vcd
  (waveforms
    (c-valid 1)
    (c-ready 1)
    (c-data  8)
    (m-full  1)
    (m-write 1)
    (p-valid 1)
    (p-ready 1)
    (p-data  8))
  10 "10 ns"
  (open-output-file "fifo.vcd" #:exists 'replace))
