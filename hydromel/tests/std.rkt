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

(define-syntax-parse-rule (test-function/exn (name arg ...))
  (test-exn (format "~a~a" 'name '(arg ...)) exn:fail? (thunk (call name arg ...))))

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
; _if_
; ------------------------------------------------------------------------------

(test-function (_if_ 1 10 20) 10)
(test-function (_if_ 0 10 20) 20)
(test-function (_if_ 8 10 20) 10)

(test-function (_if_ 1 10 1 20 1 30 40) 10)
(test-function (_if_ 0 10 1 20 1 30 40) 20)
(test-function (_if_ 0 10 0 20 1 30 40) 30)
(test-function (_if_ 0 10 0 20 0 30 40) 40)

(test-return-type (_if_ (t/static-data/literal 1) (t/unsigned 4) (t/unsigned 8)) (t/unsigned 4))
(test-return-type (_if_ (t/static-data/literal 0) (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-return-type (_if_ (t/unsigned 1) (t/unsigned 4) (t/unsigned 8)) (t/union (list (t/unsigned 4) (t/unsigned 8))))

(test-return-type (_if_ (t/static-data/literal 1) (t/unsigned 4)
                        (t/static-data/literal 1) (t/unsigned 8)
                                                  (t/unsigned 12)) (t/unsigned 4))
(test-return-type (_if_ (t/static-data/literal 1) (t/unsigned 4)
                        (t/unsigned 1)            (t/unsigned 8)
                                                  (t/unsigned 12)) (t/unsigned 4))
(test-return-type (_if_ (t/static-data/literal 0) (t/unsigned 4)
                        (t/static-data/literal 1) (t/unsigned 8)
                                                  (t/unsigned 12)) (t/unsigned 8))
(test-return-type (_if_ (t/static-data/literal 0) (t/unsigned 4)
                        (t/static-data/literal 0) (t/unsigned 8)
                                                  (t/unsigned 12)) (t/unsigned 12))
(test-return-type (_if_ (t/static-data/literal 0) (t/unsigned 4)
                        (t/unsigned 1)            (t/unsigned 8)
                                                  (t/unsigned 12)) (t/union (list (t/unsigned 8) (t/unsigned 12))))

; ------------------------------------------------------------------------------
; _case_
; ------------------------------------------------------------------------------

(test-function (_case_ 10 '(10) 1 '(20) 2 '(30) 3) 1)
(test-function (_case_ 20 '(10) 1 '(20) 2 '(30) 3) 2)
(test-function (_case_ 30 '(10) 1 '(20) 2 '(30) 3) 3)
(test-function (_case_ 40 '(10) 1 '(20) 2 '(30) 3 4) 4)
(test-function/exn (_case_ 40 '(10) 1 '(20) 2 '(30) 3))

(test-function (_case_ 15 '(10 15) 1 '(20 25) 2 '(30 35) 3) 1)
(test-function (_case_ 25 '(10 15) 1 '(20 25) 2 '(30 35) 3) 2)
(test-function (_case_ 35 '(10 15) 1 '(20 25) 2 '(30 35) 3) 3)
(test-function (_case_ 40 '(10 15) 1 '(20 25) 2 '(30 35) 3 4) 4)
(test-function/exn (_case_ 40 '(10 15) 1 '(20 25) 2 '(30 35) 3))

(test-return-type (_case_ (t/static-data/literal 10) (t/tuple (list (t/static-data/literal 10))) (t/unsigned 4) (t/tuple (list (t/static-data/literal 20))) (t/unsigned 8) (t/tuple (list (t/static-data/literal 30))) (t/unsigned 12))
                  (t/unsigned 4))
(test-return-type (_case_ (t/static-data/literal 20) (t/tuple (list (t/static-data/literal 10))) (t/unsigned 4) (t/tuple (list (t/static-data/literal 20))) (t/unsigned 8) (t/tuple (list (t/static-data/literal 30))) (t/unsigned 12))
                  (t/unsigned 8))
(test-return-type (_case_ (t/static-data/literal 30) (t/tuple (list (t/static-data/literal 10))) (t/unsigned 4) (t/tuple (list (t/static-data/literal 20))) (t/unsigned 8) (t/tuple (list (t/static-data/literal 30))) (t/unsigned 12))
                  (t/unsigned 12))
(test-return-type (_case_ (t/static-data/literal 40) (t/tuple (list (t/static-data/literal 10))) (t/unsigned 4) (t/tuple (list (t/static-data/literal 20))) (t/unsigned 8) (t/tuple (list (t/static-data/literal 30))) (t/unsigned 12) (t/unsigned 16))
                  (t/unsigned 16))
(test-return-type/exn (_case_ (t/static-data/literal 40) (t/tuple (list (t/static-data/literal 10))) (t/unsigned 4) (t/tuple (list (t/static-data/literal 20))) (t/unsigned 8) (t/tuple (list (t/static-data/literal 30))) (t/unsigned 12)))

(test-return-type (_case_ (t/any) (t/any) (t/unsigned 4) (t/any) (t/unsigned 8) (t/any) (t/unsigned 12))
                  (t/union (list (t/unsigned 4) (t/unsigned 8) (t/unsigned 12))))
(test-return-type (_case_ (t/any) (t/any) (t/unsigned 4) (t/any) (t/unsigned 8) (t/any) (t/unsigned 12) (t/unsigned 16))
                  (t/union (list (t/unsigned 4) (t/unsigned 8) (t/unsigned 12) (t/unsigned 16))))

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
; _set_slice_
; ------------------------------------------------------------------------------

(test-function (_set_slice_  #xAB03 11 8 4)       #xA403)
(test-function (_set_slice_  #xAB03 11 8 #xFFF4)  #xA403)
(test-function (_set_slice_  #xAB03 11 8 #x-C)    #xA403)
(test-function (_set_slice_  #xAB03 11 8 4 3 0 6) #xA406)
(test-function (_set_slice_ #x-AB03 11 8 3)       #x-AC03)
(test-function (_set_slice_ #x-AB03 11 8 #xFFF3)  #x-AC03)
(test-function (_set_slice_ #x-AB03 11 8 #x-D)    #x-AC03)

(test-return-type (_set_slice_ (t/unsigned 16) (t/static-data/literal 11) (t/static-data/literal 8) (t/static-data/literal 4)) (t/unsigned 16))
(test-return-type (_set_slice_ (t/signed 16) (t/static-data/literal 11) (t/static-data/literal 8) (t/static-data/literal 4)) (t/signed 16))

(test-return-type/exn (_set_slice_ (t/any) (t/static-data/literal 11) (t/static-data/literal 8) (t/static-data/literal 4)))
(test-return-type/exn (_set_slice_ (t/signed 16) (t/any) (t/static-data/literal 8) (t/static-data/literal 4)))
(test-return-type/exn (_set_slice_ (t/signed 16) (t/static-data/literal 11) (t/any) (t/static-data/literal 4)))
(test-return-type/exn (_set_slice_ (t/signed 16) (t/static-data/literal 11) (t/static-data/literal 8) (t/any)))

; ------------------------------------------------------------------------------
; _concat_
; ------------------------------------------------------------------------------

(test-function (_concat_) 0)
(test-function (_concat_ #xA (t/unsigned 4) #xB (t/unsigned 8) #xFC (t/unsigned 8)) #xA0BFC)
(test-function (_concat_ #xA (t/signed   4) #xB (t/unsigned 8) #xFC (t/unsigned 8)) #x-5F404)
(test-function (_concat_ #xA (t/unsigned 4) #xB (t/signed   8) #xFC (t/signed 8))   #xA0BFC)
(test-function (_concat_ #xA (t/signed   4) #xB (t/signed   8) #xFC (t/signed 8))   #x-5F404)

(test-return-type (_concat_) (t/static-data 0 (t/unsigned 1)))

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
; _array_
; ------------------------------------------------------------------------------

(test-function (_array_) (pvector))
(test-function (_array_ 10) (pvector 10))
(test-function (_array_ 10 20 30) (pvector 10 20 30))

(test-return-type (_array_ (t/unsigned 1) (t/unsigned 4) (t/unsigned 8))
                  (t/array 3 (t/union (list (t/unsigned 1) (t/unsigned 4) (t/unsigned 8)))))

; ------------------------------------------------------------------------------
; _record_
; ------------------------------------------------------------------------------

(test-function (_record_ 'x 12 'y 50) #hash((x . 12) (y . 50)))

(test-return-type (_record_ (t/static-data/literal 'x) (t/unsigned 4) (t/static-data/literal 'y) (t/unsigned 6))
                  (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 6))))

(test-return-type/exn (_record_ (t/unsigned 4) (t/unsigned 8)))

; ------------------------------------------------------------------------------
; _nth_
; ------------------------------------------------------------------------------

(test-function (_nth_ (pvector 10 20 30) 0) 10)
(test-function (_nth_ (pvector 10 20 30) 1) 20)
(test-function (_nth_ (pvector 10 20 30) 2) 30)

(test-function (_nth_ (pvector
                        (pvector 10 20 30)
                        (pvector 40 50 60)
                        (pvector 70 80 90)) 1 2) 60)

(test-function (_nth_ (pvector
                        (pvector 10 20 30)
                        (pvector 40 50 60)
                        (pvector 70 80 90)) 1) (pvector 40 50 60))

(test-function/exn (_nth_ (pvector 10 20 30) -1))
(test-function/exn (_nth_ (pvector 10 20 30) 3))
(test-function/exn (_nth_ (pvector 10 20 30) 'x))

(test-return-type (_nth_ (t/array 3 (t/unsigned 4)) (t/unsigned 2)) (t/unsigned 4))
(test-return-type/exn (_nth_ (t/array 3 (t/unsigned 4)) (t/symbol 'x)))

(test-return-type (_nth_ (t/make-array:impl 2 3 (t/unsigned 4)) (t/unsigned 2)) (t/array 3 (t/unsigned 4)))
(test-return-type (_nth_ (t/make-array:impl 2 3 (t/unsigned 4)) (t/unsigned 2) (t/unsigned 2)) (t/unsigned 4))

(test-return-type/exn (_nth_ (t/unsigned 8) (t/unsigned 4)))
(test-return-type/exn (_nth_ (t/unsigned 8) (t/symbol 'x)))

; ------------------------------------------------------------------------------
; _set_nth_
; ------------------------------------------------------------------------------

(test-function (_set_nth_ (pvector 10 20 30) 0 55) (pvector 55 20 30))
(test-function (_set_nth_ (pvector 10 20 30) 1 55) (pvector 10 55 30))
(test-function (_set_nth_ (pvector 10 20 30) 2 55) (pvector 10 20 55))
(test-function (_set_nth_ (pvector 10 20 30) 0 55 2 66) (pvector 55 20 66))

(test-function (_set_nth_ (pvector
                            (pvector 10 20 30)
                            (pvector 40 50 60)
                            (pvector 70 80 90)) (list 1) (pvector 14 15 16))
               (pvector
                 (pvector 10 20 30)
                 (pvector 14 15 16)
                 (pvector 70 80 90)))

(test-function (_set_nth_ (pvector
                            (pvector 10 20 30)
                            (pvector 40 50 60)
                            (pvector 70 80 90)) (list 1 2) 16 (list 2 1) 18)
               (pvector
                 (pvector 10 20 30)
                 (pvector 40 50 16)
                 (pvector 70 18 90)))


(test-function/exn (_set_nth_ (pvector 10 20 30) -1 55))
(test-function/exn (_set_nth_ (pvector 10 20 30)  3 55))
(test-function/exn (_set_nth_ (pvector 10 20 30) 'x 55))

(test-return-type (_set_nth_ (t/array 3 (t/unsigned 4)) (t/unsigned 2) (t/unsigned 3)) (t/array 3 (t/unsigned 4)))
(test-return-type (_set_nth_ (t/make-array:impl 2 3 (t/unsigned 4)) (t/tuple (list (t/unsigned 2) (t/unsigned 2))) (t/unsigned 3)) (t/make-array:impl 2 3 (t/unsigned 4)))
(test-return-type/exn (_set_nth_ (t/array 3 (t/unsigned 4)) (t/unsigned 2) (t/unsigned 5)))
(test-return-type/exn (_set_nth_ (t/array 3 (t/unsigned 4)) (t/unsigned 2) (t/symbol 'x)))
(test-return-type/exn (_set_nth_ (t/array 3 (t/unsigned 4)) (t/symbol 'x) (t/unsigned 3)))

; TODO multidimensional arrays

; ------------------------------------------------------------------------------
; _field_
; ------------------------------------------------------------------------------

(test-function (_field_ (hash 'x 10 'y 20) 'x) 10)
(test-function (_field_ (hash 'x 10 'y 20) 'y) 20)
(test-function/exn (_field_ (hash 'x 10 'y 20) 'z))

(test-return-type (_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8))) (t/static-data/literal 'x)) (t/unsigned 4))
(test-return-type (_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8))) (t/static-data/literal 'y)) (t/unsigned 8))
(test-return-type/exn (_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8))) (t/static-data/literal 'z)))
(test-return-type/exn (_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8))) (t/static-data/literal 12)))
(test-return-type/exn (_field_ (t/any) (t/static-data/literal 'z)))

; ------------------------------------------------------------------------------
; _set_field_
; ------------------------------------------------------------------------------

(test-function (_set_field_ (hash 'x 10 'y 20) 'x 55) (hash 'x 55 'y 20))
(test-function (_set_field_ (hash 'x 10 'y 20) 'y 55) (hash 'x 10 'y 55))
(test-function (_set_field_ (hash 'x 10 'y 20 'z 30) 'x 55 'z 66) (hash 'x 55 'y 20 'z 66))

(test-return-type (_set_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8)))
                               (t/static-data/literal 'x) (t/unsigned 3))
                  (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8))))
(test-return-type (_set_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8)))
                               (t/static-data/literal 'y) (t/unsigned 7))
                  (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8))))
(test-return-type/exn (_set_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8)))
                                   (t/static-data/literal 'z) (t/unsigned 3)))
(test-return-type/exn (_set_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8)))
                                   (t/static-data/literal 'x) (t/unsigned 5)))
(test-return-type/exn (_set_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8)))
                                   (t/static-data/literal 'y) (t/unsigned 9)))
(test-return-type/exn (_set_field_ (t/record (hash 'x (t/unsigned 4) 'y (t/unsigned 8)))
                                   (t/static-data/literal 12) (t/unsigned 3)))
(test-return-type/exn (_set_field_ (t/any)
                                   (t/static-data/literal 'x) (t/unsigned 3)))

; ------------------------------------------------------------------------------
; _cast_
; ------------------------------------------------------------------------------

(test-function (_cast_ (t/unsigned 8) 36) 36)
(test-function (_cast_ (t/unsigned 4) 36) 4)
(test-function (_cast_ (t/unsigned 8) -36) 220)
(test-function (_cast_ (t/signed 8) 36) 36)
(test-function (_cast_ (t/signed 3) 36) -4)
(test-function (_cast_ (t/signed 8) -36) -36)
(test-function (_cast_ (t/any) 'x) 'x)
(test-function/exn (_cast_ (t/signed 8) 'x))

(test-function (_cast_ (t/signed #f) 36) -28)
(test-function (_cast_ (t/signed #f) -36) -36)
(test-function (_cast_ (t/unsigned #f) 36) 36)
(test-function (_cast_ (t/unsigned #f) -36) 92)

(test-return-type (_cast_ (t/static-data/literal (t/unsigned 8)) (t/unsigned 5)) (t/unsigned 8))
(test-return-type (_cast_ (t/static-data/literal (t/unsigned 4)) (t/unsigned 5)) (t/unsigned 4))
(test-return-type (_cast_ (t/static-data/literal (t/unsigned 8)) (t/signed 5)) (t/unsigned 8))
(test-return-type (_cast_ (t/static-data/literal (t/signed 8)) (t/unsigned 5)) (t/signed 8))
(test-return-type (_cast_ (t/static-data/literal (t/signed 3)) (t/unsigned 5)) (t/signed 3))
(test-return-type (_cast_ (t/static-data/literal (t/signed 8)) (t/signed 5)) (t/signed 8))
(test-return-type (_cast_ (t/static-data/literal (t/unsigned #f)) (t/unsigned 5)) (t/unsigned 5))
(test-return-type (_cast_ (t/static-data/literal (t/unsigned #f)) (t/signed 5)) (t/unsigned 5))
(test-return-type (_cast_ (t/static-data/literal (t/signed #f)) (t/unsigned 5)) (t/signed 5))
(test-return-type (_cast_ (t/static-data/literal (t/signed #f)) (t/signed 5)) (t/signed 5))

(test-return-type/exn (_cast_ (t/static-data/literal 40) (t/unsigned 5)))
(test-return-type/exn (_cast_ (t/static-data/literal (t/unsigned #f)) (t/any)))
(test-return-type/exn (_cast_ (t/static-data/literal (t/signed #f)) (t/any)))

; ------------------------------------------------------------------------------
; _range_
; ------------------------------------------------------------------------------

(test-function (_range_ 0 3) (range 4))
(test-function (_range_ -3 3) (range -3 4))
(test-function (_range_ 3 0) (range 3 -1 -1))
(test-function (_range_ 3 -3) (range 3 -4 -1))

(test-return-type (_range_ (t/unsigned 4) (t/unsigned 8)) (t/range (t/unsigned 8)))
(test-return-type (_range_ (t/signed   4) (t/unsigned 8)) (t/range (t/signed   9)))
(test-return-type (_range_ (t/signed   4) (t/signed   8)) (t/range (t/signed   8)))

(test-return-type/exn (_range_ (t/any)        (t/unsigned 8)))
(test-return-type/exn (_range_ (t/unsigned 8) (t/any)))
(test-return-type/exn (_range_ (t/any)        (t/any)))
