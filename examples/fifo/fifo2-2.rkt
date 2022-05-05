; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/runtime-path
  hydromel/support
  "../common.rkt"
  "fifo2-2.mel"
  "fifo2-tests.rkt")

(define inst (fifo2 (unsigned 8)))

(instance-set! inst '(c valid) (list->signal c_valid-in))
(instance-set! inst '(c data)  (list->signal c_data-in))
(instance-set! inst '(p ready) (list->signal p_ready-in))

(test-signal inst '(c ready) c_ready-exp)
(test-signal inst '(p valid) p_valid-exp)
(test-signal inst '(p data)  p_data-exp)

(define-runtime-path vcd-file "fifo2-2.vcd")

(instance-dump-vcd inst (length c_valid-in) "10 ns"
  (open-output-file vcd-file #:exists 'replace))
