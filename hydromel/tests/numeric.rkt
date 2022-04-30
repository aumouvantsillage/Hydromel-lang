; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  "../lib/numeric.rkt")

; Construction
(define (test-unsigned-width v w)
  (test-equal? (format "Min width of ~au is ~a" v w) (min-unsigned-width v) w))

(test-unsigned-width 0    1)
(test-unsigned-width 1    1)
(test-unsigned-width 2    2)
(test-unsigned-width 3    2)
(test-unsigned-width 4    3)
(test-unsigned-width 128  8)
(test-unsigned-width 255  8)
(test-unsigned-width 256  9)
(test-unsigned-width -1   1)
(test-unsigned-width -2   2)
(test-unsigned-width -3   3)
(test-unsigned-width -4   3)
(test-unsigned-width -5   4)
(test-unsigned-width -127 8)
(test-unsigned-width -128 8)
(test-unsigned-width -129 9)

(define (test-signed-width v w)
  (test-equal? (format "Min width of ~as is ~a" v w) (min-signed-width v) w))

(test-signed-width 0    1)
(test-signed-width 1    2)
(test-signed-width 2    3)
(test-signed-width 3    3)
(test-signed-width 4    4)
(test-signed-width 128  9)
(test-signed-width 255  9)
(test-signed-width 256  10)
(test-signed-width -1   1)
(test-signed-width -2   2)
(test-signed-width -3   3)
(test-signed-width -4   3)
(test-signed-width -5   4)
(test-signed-width -127 8)
(test-signed-width -128 8)
(test-signed-width -129 9)

(test-equal? "u'#x0A[3..0] = 10" (unsigned       #x0A 4)   10)
(test-equal? "s'#x0A[3..0] = -6" (signed         #x0A 4)   -6)
(test-equal? "u'#x05[3..0] =  5" (unsigned       #x05 4)    5)
(test-equal? "s'#x05[3..0] =  5" (signed         #x05 4)    5)

(test-equal? "u'#xA0[7..4] = 10" (unsigned-slice #xA0 7 4) 10)
(test-equal? "s'#xA0[7..4] = -6" (signed-slice   #xA0 7 4) -6)
(test-equal? "u'#x50[7..4] =  5" (unsigned-slice #x50 7 4)  5)
(test-equal? "s'#x50[7..4] =  5" (signed-slice   #x50 7 4)  5)

(test-equal? "u'#xA7[5..2]<-6 = #x9B" (set-slice #xA7 5 2 6) #x9B)
(test-equal? "s'#xA7[5..2]<-6 = #x9B" (set-slice #x-59 5 2 6) #x-65)

(check-equal? (unsigned-concat [#xA0 7 5] [#x04 2 2] [#x05 3 0]) #xB5)
(check-equal? (unsigned-concat [#x60 7 5] [#x04 2 2] [#x05 3 0]) #x75)
(check-equal? (signed-concat   [#xA0 7 5] [#x04 2 2] [#x05 3 0]) (- #xB5 #x100))
(check-equal? (signed-concat   [#x60 7 5] [#x04 2 2] [#x05 3 0]) #x75)
(check-equal? (signed-concat   [#x60 3 3] [#x04 2 2] [#x05 3 0]) #x15)
(check-equal? (signed-concat   [#x60 5 5] [#x04 2 2] [#x05 3 0]) (- #x15 #x20))
