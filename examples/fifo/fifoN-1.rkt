; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/runtime-path
  hydromel
  hydromel/lib/signal
  hydromel/lib/helpers
  hydromel/lib/vcd
  "fifoN-1.mel"
  "fifo3-tests.rkt")

(define inst (fifo-make (unsigned-type 8) 3))

(slot-set! (inst c valid) (list->signal c_valid-in))
(slot-set! (inst c data)  (list->signal c_data-in))
(slot-set! (inst p ready) (list->signal p_ready-in))

(test-signal "fifo.c.ready" (slot-ref inst c ready) c_ready-exp)
(test-signal "fifo.p.valid" (slot-ref inst p valid) p_valid-exp)
(test-signal "fifo.p.data"  (slot-ref inst p data)  p_data-exp)

(define-runtime-path vcd-file "fifoN-1.vcd")

(dump-vcd inst (length c_valid-in) "10 ns"
  (open-output-file vcd-file #:exists 'replace))
