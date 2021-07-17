#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/helpers
  "full_adder.hdrml")

(define inst (full_adder-make-instance))

(port-set! (inst full_adder-a)  (signal #f #f #f #f #t #t #t #t))
(port-set! (inst full_adder-b)  (signal #f #f #t #t #f #f #t #t))
(port-set! (inst full_adder-ci) (signal #f #t #f #t #f #t #f #t))

(define s  (signal-take (port-ref inst full_adder-s)  8))
(define co (signal-take (port-ref inst full_adder-co) 8))

(printf "s  = ~a\n" s)
(printf "co = ~a\n" co)
