; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  syntax/parse/define
  data/pvector
  "../lib/std.rkt"
  "../lib/expander.rkt"
  (prefix-in t/ "../lib/types.rkt")
  (for-syntax
    racket/match
    racket/syntax
    (prefix-in meta/ "../lib/meta.rkt")))

(define-syntax-parse-rule (test-function (name arg ...) res)
  (test-equal? (format "~a~a" 'name '(arg ...)) (call name arg ...) res))

(define-syntax-parse-rule (test-return-type (name arg ...) res)
  (test-equal? (format "~a:return-type~a" 'name '(arg ...)) (call:return-type name arg ...) res))

(define-syntax-parse-rule (test-return-type/exn (name arg ...))
  (test-exn (format "~a:return-type~a" 'name '(arg ...)) exn:fail? (thunk (call:return-type name arg ...))))

(define-syntax-parser call
  [(_ name arg ...)
   (match-define (struct meta/function (fn-name cast?)) (syntax-local-value #'name))
   (if cast?
     #`(let ([rt (call:return-type name (t/static-data/literal arg) ...)])
         (rt (#,fn-name arg ...)))
     #`(#,fn-name arg ...))])

(define-syntax-parse-rule (call:return-type name arg ...)
   #:with fn-name (meta/function-name (syntax-local-value #'name))
   #:with rt-name (format-id #'fn-name "~a:return-type" #'fn-name)
   (rt-name arg ...))

; Test that literal-type(f(x ...)) <: f:return-type(literal-type(x) ...)
; This does not test that f:return-type has the minimal width.
(define-syntax-rule (test-return-type/accept (name arg ...))
  (test-true (format "~a:return-type~a" 'name '(arg ...))
             (t/<: (t/literal-type (call name arg ...))
                   (call:return-type name (t/static-data/literal arg) ...))))

; Test that literal-type(f(x ...)) = f:return-type(literal-type(x) ...)
(define-syntax-rule (test-return-type/strict (name arg ...))
  (test-equal? (format "~a:return-type~a" 'name '(arg ...))
               (t/normalize-type (call:return-type name (t/static-data/literal arg) ...))
               (t/literal-type (call name arg ...))))

; ------------------------------------------------------------------------------
; int->bool
; ------------------------------------------------------------------------------

(test-function (int->bool 0) #f)
(test-function (int->bool 1) #t)
(test-function (int->bool 2) #t)

(test-return-type     (int->bool (t/abstract-integer #f)) (t/boolean))
(test-return-type/exn (int->bool (t/symbol 'X)))

; ------------------------------------------------------------------------------
; TODO _if_
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; TODO _case_
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; _not_
; ------------------------------------------------------------------------------

(test-function (_not_  0) 1)
(test-function (_not_  1) 0)
(test-function (_not_  2) 1)
(test-function (_not_  3) 0)
(test-function (_not_  4) 3)
(test-function (_not_  5) 2)
(test-function (_not_ -1) 0)
(test-function (_not_ -2) 1)
(test-function (_not_ -3) 2)
(test-function (_not_ -4) 3)

(test-return-type     (_not_ (t/unsigned 1)) (t/unsigned 1))
(test-return-type     (_not_ (t/unsigned 2)) (t/unsigned 2))
(test-return-type     (_not_ (t/signed 1))   (t/signed 1))
(test-return-type     (_not_ (t/signed 2))   (t/signed 2))
(test-return-type/exn (_not_ (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _and_
; ------------------------------------------------------------------------------

(test-function (_and_    0     0)    0)
(test-function (_and_    0     1)    0)
(test-function (_and_    1     0)    0)
(test-function (_and_    1     1)    1)
(test-function (_and_ #xF0  #x3C) #x30)
(test-function (_and_   -1    12)   12)
(test-function (_and_  -24    12)    8)
(test-function (_and_  -24    -4)  -24)

(test-return-type     (_and_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_and_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 8))
(test-return-type     (_and_ (t/unsigned 8) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_and_ (t/signed   4) (t/signed   8)) (t/signed   8))
(test-return-type     (_and_ (t/signed   8) (t/signed   4)) (t/signed   8))
(test-return-type     (_and_ (t/signed   8) (t/signed   8)) (t/signed   8))
(test-return-type     (_and_ (t/signed   4) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_and_ (t/unsigned 4) (t/signed   8)) (t/unsigned 8))
(test-return-type     (_and_ (t/signed   8) (t/unsigned 4)) (t/unsigned 8))
(test-return-type     (_and_ (t/unsigned 8) (t/signed   4)) (t/unsigned 8))
(test-return-type     (_and_ (t/unsigned 8) (t/signed   8)) (t/unsigned 8))
(test-return-type     (_and_ (t/signed   8) (t/unsigned 8)) (t/unsigned 8))
(test-return-type/exn (_and_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_and_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _or_
; ------------------------------------------------------------------------------

(test-function (_or_    0    0)    0)
(test-function (_or_    0    1)    1)
(test-function (_or_    1    0)    1)
(test-function (_or_    1    1)    1)
(test-function (_or_ #xF0 #x3C) #xFC)
(test-function (_or_   -1   12)   -1)
(test-function (_or_  -24   12)  -20)
(test-function (_or_  -24   -4)   -4)

(test-return-type     (_or_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_or_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 8))
(test-return-type     (_or_ (t/unsigned 8) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_or_ (t/signed   4) (t/signed   8)) (t/signed   8))
(test-return-type     (_or_ (t/signed   8) (t/signed   4)) (t/signed   8))
(test-return-type     (_or_ (t/signed   8) (t/signed   8)) (t/signed   8))
(test-return-type     (_or_ (t/signed   4) (t/unsigned 8)) (t/signed   8))
(test-return-type     (_or_ (t/unsigned 4) (t/signed   8)) (t/signed   8))
(test-return-type     (_or_ (t/signed   8) (t/unsigned 4)) (t/signed   8))
(test-return-type     (_or_ (t/unsigned 8) (t/signed   4)) (t/signed   8))
(test-return-type     (_or_ (t/unsigned 8) (t/signed   8)) (t/signed   8))
(test-return-type     (_or_ (t/signed   8) (t/unsigned 8)) (t/signed   8))
(test-return-type/exn (_or_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_or_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _xor_
; ------------------------------------------------------------------------------

(test-function (_xor_    0    0)    0)
(test-function (_xor_    0    1)    1)
(test-function (_xor_    1    0)    1)
(test-function (_xor_    1    1)    0)
(test-function (_xor_ #xF0 #x3C) #xCC)
(test-function (_xor_   -1   12)  -13)
(test-function (_xor_  -24   12)  -28)
(test-function (_xor_  -24   -4)   20)

(test-return-type     (_xor_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_xor_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 8))
(test-return-type     (_xor_ (t/unsigned 8) (t/unsigned 8)) (t/unsigned 8))
(test-return-type     (_xor_ (t/signed   4) (t/signed   8)) (t/signed   8))
(test-return-type     (_xor_ (t/signed   8) (t/signed   4)) (t/signed   8))
(test-return-type     (_xor_ (t/signed   8) (t/signed   8)) (t/signed   8))
(test-return-type     (_xor_ (t/signed   4) (t/unsigned 8)) (t/signed   9))
(test-return-type     (_xor_ (t/unsigned 4) (t/signed   8)) (t/signed   8))
(test-return-type     (_xor_ (t/signed   8) (t/unsigned 4)) (t/signed   8))
(test-return-type     (_xor_ (t/unsigned 8) (t/signed   4)) (t/signed   9))
(test-return-type     (_xor_ (t/unsigned 8) (t/signed   8)) (t/signed   9))
(test-return-type     (_xor_ (t/signed   8) (t/unsigned 8)) (t/signed   9))
(test-return-type/exn (_xor_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_xor_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; unsigned_width
; ------------------------------------------------------------------------------

(test-function (unsigned_width 0)    1)
(test-function (unsigned_width 1)    1)
(test-function (unsigned_width 2)    2)
(test-function (unsigned_width 3)    2)
(test-function (unsigned_width 4)    3)
(test-function (unsigned_width 128)  8)
(test-function (unsigned_width 255)  8)
(test-function (unsigned_width 256)  9)
(test-function (unsigned_width -1)   1)
(test-function (unsigned_width -2)   2)
(test-function (unsigned_width -3)   3)
(test-function (unsigned_width -4)   3)
(test-function (unsigned_width -5)   4)
(test-function (unsigned_width -127) 8)
(test-function (unsigned_width -128) 8)
(test-function (unsigned_width -129) 9)

(for ([n (in-range -129 256)])
  (test-return-type/strict (unsigned_width n)))

(test-return-type/exn (unsigned_width (t/symbol 'X)))

; ------------------------------------------------------------------------------
; signed_width
; ------------------------------------------------------------------------------

(test-function (signed_width 0)    1)
(test-function (signed_width 1)    2)
(test-function (signed_width 2)    3)
(test-function (signed_width 3)    3)
(test-function (signed_width 4)    4)
(test-function (signed_width 128)  9)
(test-function (signed_width 255)  9)
(test-function (signed_width 256)  10)
(test-function (signed_width -1)   1)
(test-function (signed_width -2)   2)
(test-function (signed_width -3)   3)
(test-function (signed_width -4)   3)
(test-function (signed_width -5)   4)
(test-function (signed_width -127) 8)
(test-function (signed_width -128) 8)
(test-function (signed_width -129) 9)

(for ([n (in-range -129 256)])
  (if (< n 0)
    (test-return-type/strict (signed_width n))
    (test-return-type/accept (signed_width n)))) ; TODO Add tests in strict mode for n ≥ 0

(test-return-type/exn (signed_width (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _==_
; ------------------------------------------------------------------------------

(test-function (_==_ 10 20) 0)
(test-function (_==_ 10 10) 1)
(test-function (_==_ 'X 10) 0)
(test-function (_==_ 'X 'Y) 0)
(test-function (_==_ 'X 'X) 1)
(test-function (_==_ (pvector 1 2) 10) 0)
(test-function (_==_ (pvector 1 2) (pvector 3 4)) 0)
(test-function (_==_ (pvector 1 2) (pvector 1 2)) 1)

(test-return-type (_==_ (t/any) (t/any)) (t/unsigned 1))

; ------------------------------------------------------------------------------
; _/=_
; ------------------------------------------------------------------------------

(test-function (_/=_ 10 20) 1)
(test-function (_/=_ 10 10) 0)
(test-function (_/=_ 'X 10) 1)
(test-function (_/=_ 'X 'Y) 1)
(test-function (_/=_ 'X 'X) 0)
(test-function (_/=_ (pvector 1 2) 10) 1)
(test-function (_/=_ (pvector 1 2) (pvector 3 4)) 1)
(test-function (_/=_ (pvector 1 2) (pvector 1 2)) 0)

(test-return-type (_/=_ (t/any) (t/any)) (t/unsigned 1))

; ------------------------------------------------------------------------------
; _>_
; ------------------------------------------------------------------------------

(test-function (_>_ -5 -5) 0)
(test-function (_>_ -5 10) 0)
(test-function (_>_ 10 -5) 1)
(test-function (_>_ 10 10) 0)

(test-return-type (_>_ (t/signed 4) (t/unsigned 5)) (t/unsigned 1))

(test-return-type/exn (_>_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_>_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _<_
; ------------------------------------------------------------------------------

(test-function (_<_ -5 -5) 0)
(test-function (_<_ -5 10) 1)
(test-function (_<_ 10 -5) 0)
(test-function (_<_ 10 10) 0)

(test-return-type (_<_ (t/signed 4) (t/unsigned 5)) (t/unsigned 1))

(test-return-type/exn (_<_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_<_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _>=_
; ------------------------------------------------------------------------------

(test-function (_>=_ -5 -5) 1)
(test-function (_>=_ -5 10) 0)
(test-function (_>=_ 10 -5) 1)
(test-function (_>=_ 10 10) 1)

(test-return-type (_>=_ (t/signed 4) (t/unsigned 5)) (t/unsigned 1))

(test-return-type/exn (_>=_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_>=_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _<=_
; ------------------------------------------------------------------------------

(test-function (_<=_ -5 -5) 1)
(test-function (_<=_ -5 10) 1)
(test-function (_<=_ 10 -5) 0)
(test-function (_<=_ 10 10) 1)

(test-return-type (_<=_ (t/signed 4) (t/unsigned 5)) (t/unsigned 1))

(test-return-type/exn (_<=_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_<=_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _+_
; ------------------------------------------------------------------------------

(test-function (_+_ 10  200)  210)
(test-function (_+_ 10 -200) -190)

(test-return-type (_+_ (t/unsigned 4) (t/unsigned 4)) (t/unsigned 5))
(test-return-type (_+_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 9))
(test-return-type (_+_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 9))
(test-return-type (_+_ (t/signed   4) (t/signed   4)) (t/signed   5))
(test-return-type (_+_ (t/signed   4) (t/signed   8)) (t/signed   9))
(test-return-type (_+_ (t/signed   8) (t/signed   4)) (t/signed   9))
(test-return-type (_+_ (t/signed   4) (t/unsigned 4)) (t/signed   6))
(test-return-type (_+_ (t/signed   4) (t/unsigned 8)) (t/signed  10))
(test-return-type (_+_ (t/signed   8) (t/unsigned 4)) (t/signed   9))
(test-return-type (_+_ (t/unsigned 4) (t/signed   4)) (t/signed   6))
(test-return-type (_+_ (t/unsigned 4) (t/signed   8)) (t/signed   9))
(test-return-type (_+_ (t/unsigned 8) (t/signed   4)) (t/signed  10))

(test-return-type/exn (_+_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_+_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _-_
; ------------------------------------------------------------------------------

(test-function (_-_ 10  200) -190)
(test-function (_-_ 10 -200)  210)

(test-return-type (_-_ (t/unsigned 4) (t/unsigned 4)) (t/unsigned 5))
(test-return-type (_-_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 9))
(test-return-type (_-_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 9))
(test-return-type (_-_ (t/signed   4) (t/signed   4)) (t/signed   5))
(test-return-type (_-_ (t/signed   4) (t/signed   8)) (t/signed   9))
(test-return-type (_-_ (t/signed   8) (t/signed   4)) (t/signed   9))
(test-return-type (_-_ (t/signed   4) (t/unsigned 4)) (t/signed   6))
(test-return-type (_-_ (t/signed   4) (t/unsigned 8)) (t/signed  10))
(test-return-type (_-_ (t/signed   8) (t/unsigned 4)) (t/signed   9))
(test-return-type (_-_ (t/unsigned 4) (t/signed   4)) (t/signed   6))
(test-return-type (_-_ (t/unsigned 4) (t/signed   8)) (t/signed   9))
(test-return-type (_-_ (t/unsigned 8) (t/signed   4)) (t/signed  10))

(test-return-type/exn (_-_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_-_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _*_
; ------------------------------------------------------------------------------

(test-function (_*_ 10  200)  2000)
(test-function (_*_ 10 -200) -2000)

(test-return-type (_*_ (t/unsigned 4) (t/unsigned 4)) (t/unsigned  8))
(test-return-type (_*_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 12))
(test-return-type (_*_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 12))
(test-return-type (_*_ (t/signed   4) (t/signed   4)) (t/signed    8))
(test-return-type (_*_ (t/signed   4) (t/signed   8)) (t/signed   12))
(test-return-type (_*_ (t/signed   8) (t/signed   4)) (t/signed   12))
(test-return-type (_*_ (t/signed   4) (t/unsigned 4)) (t/signed    8))
(test-return-type (_*_ (t/signed   4) (t/unsigned 8)) (t/signed   12))
(test-return-type (_*_ (t/signed   8) (t/unsigned 4)) (t/signed   12))
(test-return-type (_*_ (t/unsigned 4) (t/signed   4)) (t/signed    8))
(test-return-type (_*_ (t/unsigned 4) (t/signed   8)) (t/signed   12))
(test-return-type (_*_ (t/unsigned 8) (t/signed   4)) (t/signed   12))

(test-return-type/exn (_*_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_*_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _/_
; ------------------------------------------------------------------------------

(test-function (_/_  200   10)  20)
(test-function (_/_ -200   10) -20)
(test-function (_/_  200  -10) -20)
(test-function (_/_   10  200)   0)
(test-function (_/_   10 -200)   0)
(test-function (_/_  -10  200)   0)
(test-function (_/_  -10  200)   0)

(test-return-type (_/_ (t/unsigned 4) (t/unsigned 4)) (t/unsigned  4))
(test-return-type (_/_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned  4))
(test-return-type (_/_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned  8))
(test-return-type (_/_ (t/signed   4) (t/signed   4)) (t/signed    5))
(test-return-type (_/_ (t/signed   4) (t/signed   8)) (t/signed    5))
(test-return-type (_/_ (t/signed   8) (t/signed   4)) (t/signed    9))
(test-return-type (_/_ (t/signed   4) (t/unsigned 4)) (t/signed    4))
(test-return-type (_/_ (t/signed   4) (t/unsigned 8)) (t/signed    4))
(test-return-type (_/_ (t/signed   8) (t/unsigned 4)) (t/signed    8))
(test-return-type (_/_ (t/unsigned 4) (t/signed   4)) (t/signed    5))
(test-return-type (_/_ (t/unsigned 4) (t/signed   8)) (t/signed    5))
(test-return-type (_/_ (t/unsigned 8) (t/signed   4)) (t/signed    9))

(test-return-type/exn (_/_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_/_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _neg_
; ------------------------------------------------------------------------------

(test-function (_neg_   0)   0)
(test-function (_neg_  10) -10)
(test-function (_neg_ -10)  10)

(test-return-type (_neg_ (t/unsigned 4)) (t/signed  5))
(test-return-type (_neg_ (t/signed   4)) (t/signed  5))

(test-return-type/exn (_neg_ (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _<<_
; ------------------------------------------------------------------------------

(test-function (_<<_  11  0)   11)
(test-function (_<<_  11  4)  176)
(test-function (_<<_  11 -2)    2)
(test-function (_<<_ -11  0)  -11)
(test-function (_<<_ -11  4) -176)
(test-function (_<<_ -11 -2)   -3)

(test-return-type (_<<_ (t/unsigned 4) (t/unsigned 4)) (t/unsigned  19))
(test-return-type (_<<_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 259))
(test-return-type (_<<_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned  23))
(test-return-type (_<<_ (t/signed   4) (t/signed   4)) (t/signed    11))
(test-return-type (_<<_ (t/signed   4) (t/signed   8)) (t/signed   131))
(test-return-type (_<<_ (t/signed   8) (t/signed   4)) (t/signed    15))
(test-return-type (_<<_ (t/signed   4) (t/unsigned 4)) (t/signed    19))
(test-return-type (_<<_ (t/signed   4) (t/unsigned 8)) (t/signed   259))
(test-return-type (_<<_ (t/signed   8) (t/unsigned 4)) (t/signed    23))
(test-return-type (_<<_ (t/unsigned 4) (t/signed   4)) (t/unsigned  11))
(test-return-type (_<<_ (t/unsigned 4) (t/signed   8)) (t/unsigned 131))
(test-return-type (_<<_ (t/unsigned 8) (t/signed   4)) (t/unsigned  15))

(test-return-type/exn (_<<_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_<<_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _>>_
; ------------------------------------------------------------------------------

(test-function (_>>_  177  0)   177)
(test-function (_>>_  177  4)    11)
(test-function (_>>_  177 -2)   708)
(test-function (_>>_ -177  0)  -177)
(test-function (_>>_ -177  4)   -12)
(test-function (_>>_ -177 -2)  -708)

(test-return-type (_>>_ (t/unsigned 4) (t/unsigned 4)) (t/unsigned   4))
(test-return-type (_>>_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned   4))
(test-return-type (_>>_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned   8))
(test-return-type (_>>_ (t/signed   4) (t/signed   4)) (t/signed    12))
(test-return-type (_>>_ (t/signed   4) (t/signed   8)) (t/signed   132))
(test-return-type (_>>_ (t/signed   8) (t/signed   4)) (t/signed    16))
(test-return-type (_>>_ (t/signed   4) (t/unsigned 4)) (t/signed     4))
(test-return-type (_>>_ (t/signed   4) (t/unsigned 8)) (t/signed     4))
(test-return-type (_>>_ (t/signed   8) (t/unsigned 4)) (t/signed     8))
(test-return-type (_>>_ (t/unsigned 4) (t/signed   4)) (t/unsigned  12))
(test-return-type (_>>_ (t/unsigned 4) (t/signed   8)) (t/unsigned 132))
(test-return-type (_>>_ (t/unsigned 8) (t/signed   4)) (t/unsigned  16))

(test-return-type/exn (_>>_ (t/symbol 'X)  (t/unsigned 8)))
(test-return-type/exn (_>>_ (t/unsigned 8) (t/symbol 'X)))

; ------------------------------------------------------------------------------
; _slice_
; ------------------------------------------------------------------------------

(test-function (_slice_  #xAB03 11 8) #xB)
(test-function (_slice_  #xAB03 12 8) #xB)
(test-function (_slice_ #x-AB03 11 8) #x4)
(test-function (_slice_ #x-AB03 12 8) #x-C)

(test-return-type (_slice_ (t/unsigned 16) (t/static-data/literal 11) (t/static-data/literal 8)) (t/unsigned 4))
(test-return-type (_slice_ (t/signed   16) (t/static-data/literal 11) (t/static-data/literal 8)) (t/signed 4))

; ------------------------------------------------------------------------------
; _concat_
; ------------------------------------------------------------------------------

(test-function (_concat_ #xA (t/unsigned 4) #xB (t/unsigned 8) #xFC (t/unsigned 8)) #xA0BFC)
(test-function (_concat_ #xA (t/signed   4) #xB (t/unsigned 8) #xFC (t/unsigned 8)) #x-5F404)
(test-function (_concat_ #xA (t/unsigned 4) #xB (t/signed   8) #xFC (t/signed 8))   #xA0BFC)
(test-function (_concat_ #xA (t/signed   4) #xB (t/signed   8) #xFC (t/signed 8))   #x-5F404)

(test-return-type (_concat_ (t/unsigned 1) (t/static-data/literal (t/unsigned 4))
                            (t/unsigned 2) (t/static-data/literal (t/unsigned 5))
                            (t/unsigned 3) (t/static-data/literal (t/unsigned 6)))
                  (t/unsigned 15))

(test-return-type (_concat_ (t/unsigned 1) (t/static-data/literal (t/signed   4))
                            (t/unsigned 2) (t/static-data/literal (t/unsigned 5))
                            (t/unsigned 3) (t/static-data/literal (t/unsigned 6)))
                  (t/signed 15))

(test-return-type (_concat_ (t/unsigned 1) (t/static-data/literal (t/unsigned 4))
                            (t/unsigned 2) (t/static-data/literal (t/signed   5))
                            (t/unsigned 3) (t/static-data/literal (t/signed   6)))
                  (t/unsigned 15))

(test-return-type (_concat_ (t/unsigned 1) (t/static-data/literal (t/signed 4))
                            (t/unsigned 2) (t/static-data/literal (t/signed 5))
                            (t/unsigned 3) (t/static-data/literal (t/signed 6)))
                  (t/signed 15))

; ------------------------------------------------------------------------------
; TODO test _array_
; TODO test _record_
; TODO test _nth_
; TODO test _set_nth_
; TODO test _field_
; TODO test _cast_
; TODO test _range_
