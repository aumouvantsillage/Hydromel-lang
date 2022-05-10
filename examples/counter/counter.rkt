; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  racket/runtime-path
  hydromel/support
  "../common.rkt"
  "counter.mel")

(define inst (counter 3))

(instance-set! inst 'clear  (signal 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
(instance-set! inst 'up     (signal 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0))
(instance-set! inst 'down   (signal 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0))

(test-signal   inst 'value  (list   0 1 2 3 0 1 2 2 2 1 0 3 2 1 0 3 3 0))
(test-signal   inst 'top    (list   0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 1 0))
(test-signal   inst 'bottom (list   1 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 1))

(define-runtime-path vcd-file "counter.vcd")

(define duration 19)

(instance-dump inst duration)

(instance-dump-vcd inst duration "10 ns"
  (open-output-file vcd-file #:exists 'replace))
