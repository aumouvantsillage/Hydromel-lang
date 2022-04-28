; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel/lib/signal
  hydromel/lib/instance
  "adders.mel")

; (define inst (ripple_carry_adder_rec 4))
(define inst (ripple_carry_adder_iter 4))
; (define inst (carry_lookahead_adder 11))

(instance-set! inst 'a  (signal 0 0 4 4 10 0 0 4 4 10))
(instance-set! inst 'b  (signal 0 5 0 5 12 0 5 0 5 12))
(instance-set! inst 'ci (signal 0 0 0 0  0 1 1 1 1  1))

(instance-dump inst 10)
