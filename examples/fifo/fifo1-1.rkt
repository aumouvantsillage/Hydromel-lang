; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/runtime-path
  hydromel/support
  "../common.rkt"
  "fifo1-1.mel"
  "fifo1-tests.rkt")

(define inst (fifo1 (unsigned 8)))

(instance-set! inst 'c_valid (list->signal c_valid-in))
(instance-set! inst 'c_data  (list->signal c_data-in))
(instance-set! inst 'p_ready (list->signal p_ready-in))

(test-signal inst 'c_ready c_ready-exp)
(test-signal inst 'full    full-exp)
(test-signal inst 'write   write-exp)
(test-signal inst 'r_data  r_data-exp)
(test-signal inst 'p_valid p_valid-exp)
(test-signal inst 'p_data  p_data-exp)

(define-runtime-path vcd-file "fifo1-1.vcd")

(instance-dump-vcd inst (length c_valid-in) "10 ns"
  (open-output-file vcd-file #:exists 'replace))
