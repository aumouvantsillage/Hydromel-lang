; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  hydromel/sim)

(provide test-signal)

(define (test-signal inst path lst)
  (define path-lst (if (list? path) path (list path)))
  (define path-str (string-join (for/list ([p (in-list path-lst)])
                                  (format "~a" p))
                                "."))
  (test-equal? path-str
    (signal-take (instance-ref inst path) (length lst))
    lst))
