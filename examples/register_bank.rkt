; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "register_bank.mel")

(define inst (register_bank-make (unsigned 8) 4))

(slot-set! (inst write) (signal 0  1  0  0  1  0  0  1  0  0  1  0  0  0  0  0))
(slot-set! (inst addr)  (signal 0  0  0  1  1  1  2  2  2  3  3  3  0  1  2  3))
(slot-set! (inst d)     (signal 99 10 99 99 20 99 99 30 99 99 40 99 99 99 99 99))

(define duration 16)

(define tbl (slot-table inst))

(print-slot-table tbl duration)

; (vcd tbl duration "10 ns"
;   (open-output-file "register_bank.vcd" #:exists 'replace))
