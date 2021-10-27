; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifo.mel")

(define inst (fifo1-make (unsigned 8) 0))

(slot-set! (inst c valid) (signal 0  1  0  1  1  1  0  0  0))
(slot-set! (inst c data)  (signal 10 10 10 20 30 30 30 30 30))
(slot-set! (inst p ready) (signal 1  1  0  0  0  1  0  1  0))

(define duration 10)

(define tbl (slot-table inst))

(print-slot-table tbl duration)

(vcd tbl duration "10 ns"
  (open-output-file "fifo1.vcd" #:exists 'replace))
