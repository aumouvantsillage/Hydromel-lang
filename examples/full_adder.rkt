; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "full_adder.mel")

(define inst (full_adder))

(slot-set! (inst a)  (signal 0 0 0 0 1 1 1 1))
(slot-set! (inst b)  (signal 0 0 1 1 0 0 1 1))
(slot-set! (inst ci) (signal 0 1 0 1 0 1 0 1))

(define duration 8)

(print-slot-table (slot-table inst) duration)
