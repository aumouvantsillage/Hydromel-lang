; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel/support
  "counter.mel")

(define inst (counter 3))

(instance-set! inst 'clear (signal 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
(instance-set! inst 'up    (signal 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0))
(instance-set! inst 'down  (signal 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0))

(instance-dump inst 19)
