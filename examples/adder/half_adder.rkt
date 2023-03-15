; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel/support
  "../common.rkt"
  "half_adder.mel")

(define inst (half_adder))

(instance-set! inst 'a (make-signal 0 0 1 1))
(instance-set! inst 'b (make-signal 0 1 0 1))

(test-signal   inst 's (list        0 1 1 0))
(test-signal   inst 'c (list        0 0 0 1))

(instance-dump inst 4)
