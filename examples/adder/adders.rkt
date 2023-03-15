; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel/support
  "../common.rkt"
  "adders.mel")

(define (test-inst inst)
  (instance-set! inst 'a  (make-signal 0 0 4 4 10 0 0 4 4  10))
  (instance-set! inst 'b  (make-signal 0 5 0 5 12 0 5 0 5  12))
  (instance-set! inst 'ci (make-signal 0 0 0 0 0  1 1 1 1  1))

  (test-signal   inst 's  (list        0 5 4 9 6  1 6 5 10 7))
  (test-signal   inst 'co (list        0 0 0 0 1  0 0 0 0  1)))

(define w 4)

(test-inst (ripple_carry_adder_rec  w))
(test-inst (ripple_carry_adder_iter w))
(test-inst (carry_lookahead_adder   w))
