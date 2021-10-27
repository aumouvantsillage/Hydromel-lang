; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "adders.mel")

; (define inst (ripple_carry_adder_rec-make 4))
(define inst (ripple_carry_adder_iter-make 4))
; (define inst (carry_lookahead_adder-make 11))

(slot-set! (inst a)  (signal 0 0 4 4 10 0 0 4 4 10))
(slot-set! (inst b)  (signal 0 5 0 5 12 0 5 0 5 12))
(slot-set! (inst ci) (signal 0 0 0 0  0 1 1 1 1  1))

(define duration 10)

(print-slot-table (slot-table inst) duration)
