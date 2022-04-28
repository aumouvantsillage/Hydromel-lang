; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/instance
  "full_adder.mel")

(define inst (full_adder))

(instance-set! inst 'a  (signal 0 0 0 0 1 1 1 1))
(instance-set! inst 'b  (signal 0 0 1 1 0 0 1 1))
(instance-set! inst 'ci (signal 0 1 0 1 0 1 0 1))

(instance-dump inst 8)
