#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "half_adder.hdrml")

(define inst (make-instance-half_adder))

(port-set! (inst half_adder-a) (signal #f #f #t #t))
(port-set! (inst half_adder-b) (signal #f #t #f #t))

(define s (signal-take (port-ref inst half_adder-s) 4))
(define c (signal-take (port-ref inst half_adder-c) 4))

(printf "s = ~a\n" s)
(printf "c = ~a\n" c)
