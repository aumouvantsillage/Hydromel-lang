; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  "counter.mel")

(define inst (counter-make 3))

(slot-set! (inst clear) (signal 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
(slot-set! (inst up)    (signal 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0))
(slot-set! (inst down)  (signal 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0))

(define duration 19)

(print-slot-table (slot-table inst) duration)
