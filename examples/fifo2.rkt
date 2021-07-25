#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.hdrml")

(define f (fifo2-make-instance #f 0))

(port-set! (f fifo2-c producer-valid) (signal 0  1  1  1  1  0))
(port-set! (f fifo2-c producer-data)  (signal 10 10 20 30))
(port-set! (f fifo2-p producer-ready) (signal 0  0  0  0  0  0 1 0 1 0 1 0))

(define c-valid (port-ref f fifo2-c producer-valid))
(define c-ready (port-ref f fifo2-c producer-ready))
(define c-data  (port-ref f fifo2-c producer-data))

(define m0-c-valid (port-ref f fifo2-m0 mailbox-c producer-valid))
(define m0-c-ready (port-ref f fifo2-m0 mailbox-c producer-ready))
(define m0-c-data  (port-ref f fifo2-m0 mailbox-c producer-data))
(define m0-p-valid (port-ref f fifo2-m0 mailbox-p producer-valid))
(define m0-p-ready (port-ref f fifo2-m0 mailbox-p producer-ready))
(define m0-p-data  (port-ref f fifo2-m0 mailbox-p producer-data))
(define m0-full    (port-ref f fifo2-m0 mailbox-full))
(define m0-write   (port-ref f fifo2-m0 mailbox-write))

(define m1-c-valid (port-ref f fifo2-m1 mailbox-c producer-valid))
(define m1-c-ready (port-ref f fifo2-m1 mailbox-c producer-ready))
(define m1-c-data  (port-ref f fifo2-m1 mailbox-c producer-data))
(define m1-p-valid (port-ref f fifo2-m1 mailbox-p producer-valid))
(define m1-p-ready (port-ref f fifo2-m1 mailbox-p producer-ready))
(define m1-p-data  (port-ref f fifo2-m1 mailbox-p producer-data))
(define m1-full    (port-ref f fifo2-m1 mailbox-full))
(define m1-write   (port-ref f fifo2-m1 mailbox-write))

(define p-valid (port-ref f fifo2-p producer-valid))
(define p-ready (port-ref f fifo2-p producer-ready))
(define p-data  (port-ref f fifo2-p producer-data))

(vcd
  (waveforms
    (c-valid 1)
    (c-ready 1)
    (c-data  8)
    (m0-c-valid 1)
    (m0-c-ready 1)
    (m0-c-data  8)
    (m0-p-valid 1)
    (m0-p-ready 1)
    (m0-p-data  8)
    (m0-full 1)
    (m0-write 1)
    (m1-c-valid 1)
    (m1-c-ready 1)
    (m1-c-data  8)
    (m1-p-valid 1)
    (m1-p-ready 1)
    (m1-p-data  8)
    (m1-full 1)
    (m1-write 1)
    (p-valid 1)
    (p-ready 1)
    (p-data  8))
  15 "10 ns"
  (open-output-file "fifo2.vcd" #:exists 'replace))
