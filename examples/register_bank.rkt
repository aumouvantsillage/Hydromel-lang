; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/runtime-path
  hydromel/support
  "register_bank.mel")

(define inst (register_bank (unsigned 8) 4))

(instance-set! inst 'write (signal 0  1  0  0  1  0  0  1  0  0  1  0  0  0  0  0))
(instance-set! inst 'addr  (signal 0  0  0  1  1  1  2  2  2  3  3  3  0  1  2  3))
(instance-set! inst 'd     (signal 99 10 99 99 20 99 99 30 99 99 40 99 99 99 99 99))

(define duration 16)

(instance-dump inst duration)

(define-runtime-path vcd-file "register_bank.vcd")

(instance-dump-vcd inst duration "10 ns"
  (open-output-file vcd-file #:exists 'replace))
