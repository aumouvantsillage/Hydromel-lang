; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  syntax/parse/define
  (for-syntax racket/syntax)
  data/pvector
  "std.mel")

(define-syntax-parse-rule (test-constant name val)
  (test-equal? (symbol->string 'name) name val))

; ------------------------------------------------------------------------------
; if
; ------------------------------------------------------------------------------

(test-constant if1 10)
(test-constant if2 20)
(test-constant if3 10)
(test-constant if4 10)
(test-constant if5 20)
(test-constant if6 30)
(test-constant if7 40)

; ------------------------------------------------------------------------------
; case
; ------------------------------------------------------------------------------

(test-constant case1 1)
(test-constant case2 2)
(test-constant case3 3)
(test-constant case4 4)
(test-constant case5 1)
(test-constant case6 2)
(test-constant case7 3)
(test-constant case8 4)

; ------------------------------------------------------------------------------
; not
; ------------------------------------------------------------------------------

(test-constant not1  1)
(test-constant not2  0)
(test-constant not3  1)
(test-constant not4  0)
(test-constant not5  3)
(test-constant not6  2)
(test-constant not7  0)
(test-constant not8  1)
(test-constant not9  2)
(test-constant not10 3)

; ------------------------------------------------------------------------------
; and
; ------------------------------------------------------------------------------

(test-constant and1    0)
(test-constant and2    0)
(test-constant and3    0)
(test-constant and4    1)
(test-constant and5 #x30)
(test-constant and6   12)
(test-constant and7    8)
(test-constant and8  -24)

; ------------------------------------------------------------------------------
; or
; ------------------------------------------------------------------------------

(test-constant or1    0)
(test-constant or2    1)
(test-constant or3    1)
(test-constant or4    1)
(test-constant or5 #xFC)
(test-constant or6   -1)
(test-constant or7  -20)
(test-constant or8   -4)

; ------------------------------------------------------------------------------
; xor
; ------------------------------------------------------------------------------

(test-constant xor1    0)
(test-constant xor2    1)
(test-constant xor3    1)
(test-constant xor4    0)
(test-constant xor5 #xCC)
(test-constant xor6  -13)
(test-constant xor7  -28)
(test-constant xor8   20)

; ------------------------------------------------------------------------------
; unsigned_width
; ------------------------------------------------------------------------------

(test-constant uw1  1)
(test-constant uw2  1)
(test-constant uw3  2)
(test-constant uw4  2)
(test-constant uw5  3)
(test-constant uw6  8)
(test-constant uw7  8)
(test-constant uw8  9)
(test-constant uw9  1)
(test-constant uw10 2)
(test-constant uw11 3)
(test-constant uw12 3)
(test-constant uw13 4)
(test-constant uw14 8)
(test-constant uw15 8)
(test-constant uw16 9)

; ------------------------------------------------------------------------------
; signed_width
; ------------------------------------------------------------------------------

(test-constant sw1  1)
(test-constant sw2  2)
(test-constant sw3  3)
(test-constant sw4  3)
(test-constant sw5  4)
(test-constant sw6  9)
(test-constant sw7  9)
(test-constant sw8  10)
(test-constant sw9  1)
(test-constant sw10 2)
(test-constant sw11 3)
(test-constant sw12 3)
(test-constant sw13 4)
(test-constant sw14 8)
(test-constant sw15 8)
(test-constant sw16 9)

; ------------------------------------------------------------------------------
; ==
; ------------------------------------------------------------------------------

(test-constant eq1 0)
(test-constant eq2 1)
(test-constant eq3 0)
(test-constant eq4 0)
(test-constant eq5 1)
(test-constant eq6 0)
(test-constant eq7 0)
(test-constant eq8 1)

; ------------------------------------------------------------------------------
; /=
; ------------------------------------------------------------------------------

(test-constant ne1 1)
(test-constant ne2 0)
(test-constant ne3 1)
(test-constant ne4 1)
(test-constant ne5 0)
(test-constant ne6 1)
(test-constant ne7 1)
(test-constant ne8 0)

; ------------------------------------------------------------------------------
; >
; ------------------------------------------------------------------------------

(test-constant gt1 0)
(test-constant gt2 0)
(test-constant gt3 1)
(test-constant gt4 0)

; ------------------------------------------------------------------------------
; <
; ------------------------------------------------------------------------------

(test-constant lt1 0)
(test-constant lt2 1)
(test-constant lt3 0)
(test-constant lt4 0)

; ------------------------------------------------------------------------------
; >=
; ------------------------------------------------------------------------------

(test-constant ge1 1)
(test-constant ge2 0)
(test-constant ge3 1)
(test-constant ge4 1)

; ------------------------------------------------------------------------------
; <=
; ------------------------------------------------------------------------------

(test-constant le1 1)
(test-constant le2 1)
(test-constant le3 0)
(test-constant le4 1)

; ------------------------------------------------------------------------------
; +
; ------------------------------------------------------------------------------

(test-constant add1  210)
(test-constant add2 -190)

; ------------------------------------------------------------------------------
; -
; ------------------------------------------------------------------------------

(test-constant sub1 -190)
(test-constant sub2  210)

; ------------------------------------------------------------------------------
; *
; ------------------------------------------------------------------------------

(test-constant mul1  2000)
(test-constant mul2 -2000)

; ------------------------------------------------------------------------------
; /
; ------------------------------------------------------------------------------

(test-constant div1  20)
(test-constant div2 -20)
(test-constant div3 -20)
(test-constant div4   0)
(test-constant div5   0)
(test-constant div6   0)
(test-constant div7   0)

; ------------------------------------------------------------------------------
; -()
; ------------------------------------------------------------------------------

(test-constant neg1   0)
(test-constant neg2 -10)
(test-constant neg3  10)

; ------------------------------------------------------------------------------
; <<
; ------------------------------------------------------------------------------

(test-constant shl1   11)
(test-constant shl2  176)
(test-constant shl3    2)
(test-constant shl4  -11)
(test-constant shl5 -176)
(test-constant shl6   -3)

; ------------------------------------------------------------------------------
; >>
; ------------------------------------------------------------------------------

(test-constant shr1  177)
(test-constant shr2   11)
(test-constant shr3  708)
(test-constant shr4 -177)
(test-constant shr5  -12)
(test-constant shr6 -708)

; ------------------------------------------------------------------------------
; x{}
; ------------------------------------------------------------------------------

(test-constant slice1 #xB)
(test-constant slice2 #xB)
(test-constant slice3 #x4)
(test-constant slice4 #x-C)

; ------------------------------------------------------------------------------
; <- {}
; ------------------------------------------------------------------------------

(test-constant sslice1 #xA403)
(test-constant sslice2 #xA403)
(test-constant sslice3 #xA403)
(test-constant sslice4 #xA406)
(test-constant sslice5 #x-AC03)
(test-constant sslice6 #x-AC03)
(test-constant sslice7 #x-AC03)

; ------------------------------------------------------------------------------
; {,}
; ------------------------------------------------------------------------------

(test-constant cat1 0)
(test-constant cat2 #xA)
(test-constant cat3 #xABFC)
(test-constant cat4 #xA0BFC)
(test-constant cat5 #x-5F404)
(test-constant cat6 #xA0BFC)
(test-constant cat7 #x-5F404)

; ------------------------------------------------------------------------------
; [,]
; ------------------------------------------------------------------------------

(test-constant arr1 (pvector))
(test-constant arr2 (pvector 10))
(test-constant arr3 (pvector 10 20 30))

; ------------------------------------------------------------------------------
; Array read: x[]
; ------------------------------------------------------------------------------)

(test-constant nth1 10)
(test-constant nth2 20)
(test-constant nth3 30)
(test-constant nth4 (pvector 40 50 60))
(test-constant nth5 60)

; ------------------------------------------------------------------------------
; Array assignment: x <- [=>]
; ------------------------------------------------------------------------------)

(test-constant snth1 (pvector 55 20 30))
(test-constant snth2 (pvector 10 55 30))
(test-constant snth3 (pvector 10 20 55))
(test-constant snth4 (pvector 55 20 66))
(test-constant snth5 (pvector
                       (pvector 10 20 30)
                       (pvector 14 15 16)
                       (pvector 70 80 90)))
(test-constant snth6 (pvector
                       (pvector 10 20 30)
                       (pvector 40 50 16)
                       (pvector 70 18 90)))

; ------------------------------------------------------------------------------
; Record: (=>)
; ------------------------------------------------------------------------------

(test-constant rec1 #hash((x . 12)))
(test-constant rec2 #hash((x . 12) (y . 50)))

; ------------------------------------------------------------------------------
; Record field read: .
; ------------------------------------------------------------------------------

(test-constant field1 10)
(test-constant field2 20)

; ------------------------------------------------------------------------------
; Record field assignment: <- (=>)
; ------------------------------------------------------------------------------

(test-constant sfield1 #hash((x . 55) (y . 20)))
(test-constant sfield2 #hash((x . 10) (y . 55)))
(test-constant sfield3 #hash((x . 55) (y . 20) (z . 66)))

; ------------------------------------------------------------------------------
; Cast: as
; ------------------------------------------------------------------------------

(test-constant cast1  36)
(test-constant cast2  4)
(test-constant cast3  220)
(test-constant cast4  36)
(test-constant cast5  -4)
(test-constant cast6  -36)
(test-constant cast7  -28)
(test-constant cast8  -36)
(test-constant cast9  36)
(test-constant cast10 92)
(test-constant cast11 '~x)

; ------------------------------------------------------------------------------
; Range: ..
; ------------------------------------------------------------------------------

(test-constant rng1 (range 4))
(test-constant rng2 (range -3 4))
(test-constant rng3 (range 3 -1 -1))
(test-constant rng4 (range 3 -4 -1))

; ------------------------------------------------------------------------------
; zero
; ------------------------------------------------------------------------------

(test-constant z1 0)
(test-constant z2 0)
(test-constant z3 (make-pvector 3 0))
(test-constant z4 (list 0 (make-pvector 3 0)))
(test-constant z5 (hash 'x 0 'y (make-pvector 3 0)))
(test-constant z6 '~a)
