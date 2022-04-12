; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  hydromel/lib/signal)

(provide (all-defined-out))

(define c_valid-in  (list 0  1  0  1  1  0  0  1  1  1   0   0   0   1   1   1   1   1   0   0   0   0))
(define c_ready-exp (list 1  1  1  1  1  1  1  1  1  1   1   1   1   1   1   1   0   1   1   1   1   1))
(define c_data-in   (list 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 170 180 190 200 210))
(define p_valid-exp (list 0  1  1  1  1  1  1  1  1  1   1   1   0   1   1   1   1   1   1   1   1   0))
(define p_ready-in  (list 0  0  1  0  0  1  1  0  0  1   1   1   0   0   0   0   0   1   1   1   1   0))
(define p_data-exp  (list 10 20 20 40 40 40 50 80 80 80  90  100 130 140 140 140 140 140 150 160 170 210))

(define (test-signal name sig lst)
  (test-equal? name
    (signal-take sig (length lst))
    lst))
