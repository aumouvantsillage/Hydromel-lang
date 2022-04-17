; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  syntax/parse/define
  data/pvector
  "../lib/std.rkt"
  "../lib/types.rkt"
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
     #`(let ([rt (call:return-type name (const-type/literal arg) ...)])
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
             (<: (type-of-literal (call name arg ...))
                 (call:return-type name (const-type/literal arg) ...))))

; Test that literal-type(f(x ...)) = f:return-type(literal-type(x) ...)
(define-syntax-rule (test-return-type/strict (name arg ...))
  (test-equal? (format "~a:return-type~a" 'name '(arg ...))
               (normalize (call:return-type name (const-type/literal arg) ...))
               (type-of-literal (call name arg ...))))

; ------------------------------------------------------------------------------
; int->bool
; ------------------------------------------------------------------------------

(test-function (int->bool 0) #f)
(test-function (int->bool 1) #t)
(test-function (int->bool 2) #t)

(test-return-type     (int->bool (abstract-integer-type #f)) (boolean-type))
(test-return-type/exn (int->bool (symbol-type 'X)))

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

(test-return-type (_if_ (const-type/literal 1) (unsigned-type 4) (unsigned-type 8)) (unsigned-type 4))
(test-return-type (_if_ (const-type/literal 0) (unsigned-type 4) (unsigned-type 8)) (unsigned-type 8))
(test-return-type (_if_ (unsigned-type 1) (unsigned-type 4) (unsigned-type 8)) (union-type (list (unsigned-type 4) (unsigned-type 8))))

(test-return-type (_if_ (const-type/literal 1) (unsigned-type 4)
                        (const-type/literal 1) (unsigned-type 8)
                                               (unsigned-type 12)) (unsigned-type 4))
(test-return-type (_if_ (const-type/literal 1) (unsigned-type 4)
                        (unsigned-type 1)      (unsigned-type 8)
                                               (unsigned-type 12)) (unsigned-type 4))
(test-return-type (_if_ (const-type/literal 0) (unsigned-type 4)
                        (const-type/literal 1) (unsigned-type 8)
                                               (unsigned-type 12)) (unsigned-type 8))
(test-return-type (_if_ (const-type/literal 0) (unsigned-type 4)
                        (const-type/literal 0) (unsigned-type 8)
                                               (unsigned-type 12)) (unsigned-type 12))
(test-return-type (_if_ (const-type/literal 0) (unsigned-type 4)
                        (unsigned-type 1)      (unsigned-type 8)
                                               (unsigned-type 12)) (union-type (list (unsigned-type 8) (unsigned-type 12))))

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

(test-return-type (_case_ (const-type/literal 10) (tuple-type (list (const-type/literal 10))) (unsigned-type 4) (tuple-type (list (const-type/literal 20))) (unsigned-type 8) (tuple-type (list (const-type/literal 30))) (unsigned-type 12))
                  (unsigned-type 4))
(test-return-type (_case_ (const-type/literal 20) (tuple-type (list (const-type/literal 10))) (unsigned-type 4) (tuple-type (list (const-type/literal 20))) (unsigned-type 8) (tuple-type (list (const-type/literal 30))) (unsigned-type 12))
                  (unsigned-type 8))
(test-return-type (_case_ (const-type/literal 30) (tuple-type (list (const-type/literal 10))) (unsigned-type 4) (tuple-type (list (const-type/literal 20))) (unsigned-type 8) (tuple-type (list (const-type/literal 30))) (unsigned-type 12))
                  (unsigned-type 12))
(test-return-type (_case_ (const-type/literal 40) (tuple-type (list (const-type/literal 10))) (unsigned-type 4) (tuple-type (list (const-type/literal 20))) (unsigned-type 8) (tuple-type (list (const-type/literal 30))) (unsigned-type 12) (unsigned-type 16))
                  (unsigned-type 16))
(test-return-type/exn (_case_ (const-type/literal 40) (tuple-type (list (const-type/literal 10))) (unsigned-type 4) (tuple-type (list (const-type/literal 20))) (unsigned-type 8) (tuple-type (list (const-type/literal 30))) (unsigned-type 12)))

(test-return-type (_case_ (any-type) (any-type) (unsigned-type 4) (any-type) (unsigned-type 8) (any-type) (unsigned-type 12))
                  (union-type (list (unsigned-type 4) (unsigned-type 8) (unsigned-type 12))))
(test-return-type (_case_ (any-type) (any-type) (unsigned-type 4) (any-type) (unsigned-type 8) (any-type) (unsigned-type 12) (unsigned-type 16))
                  (union-type (list (unsigned-type 4) (unsigned-type 8) (unsigned-type 12) (unsigned-type 16))))

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

(test-return-type     (_not_ (unsigned-type 1)) (unsigned-type 1))
(test-return-type     (_not_ (unsigned-type 2)) (unsigned-type 2))
(test-return-type     (_not_ (signed-type 1))   (signed-type 1))
(test-return-type     (_not_ (signed-type 2))   (signed-type 2))
(test-return-type/exn (_not_ (symbol-type 'X)))

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

(test-return-type     (_and_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_and_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type 8))
(test-return-type     (_and_ (unsigned-type 8) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_and_ (signed-type   4) (signed-type   8)) (signed-type   8))
(test-return-type     (_and_ (signed-type   8) (signed-type   4)) (signed-type   8))
(test-return-type     (_and_ (signed-type   8) (signed-type   8)) (signed-type   8))
(test-return-type     (_and_ (signed-type   4) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_and_ (unsigned-type 4) (signed-type   8)) (unsigned-type 8))
(test-return-type     (_and_ (signed-type   8) (unsigned-type 4)) (unsigned-type 8))
(test-return-type     (_and_ (unsigned-type 8) (signed-type   4)) (unsigned-type 8))
(test-return-type     (_and_ (unsigned-type 8) (signed-type   8)) (unsigned-type 8))
(test-return-type     (_and_ (signed-type   8) (unsigned-type 8)) (unsigned-type 8))
(test-return-type/exn (_and_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_and_ (unsigned-type 8) (symbol-type 'X)))

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

(test-return-type     (_or_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_or_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type 8))
(test-return-type     (_or_ (unsigned-type 8) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_or_ (signed-type   4) (signed-type   8)) (signed-type   8))
(test-return-type     (_or_ (signed-type   8) (signed-type   4)) (signed-type   8))
(test-return-type     (_or_ (signed-type   8) (signed-type   8)) (signed-type   8))
(test-return-type     (_or_ (signed-type   4) (unsigned-type 8)) (signed-type   8))
(test-return-type     (_or_ (unsigned-type 4) (signed-type   8)) (signed-type   8))
(test-return-type     (_or_ (signed-type   8) (unsigned-type 4)) (signed-type   8))
(test-return-type     (_or_ (unsigned-type 8) (signed-type   4)) (signed-type   8))
(test-return-type     (_or_ (unsigned-type 8) (signed-type   8)) (signed-type   8))
(test-return-type     (_or_ (signed-type   8) (unsigned-type 8)) (signed-type   8))
(test-return-type/exn (_or_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_or_ (unsigned-type 8) (symbol-type 'X)))

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

(test-return-type     (_xor_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_xor_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type 8))
(test-return-type     (_xor_ (unsigned-type 8) (unsigned-type 8)) (unsigned-type 8))
(test-return-type     (_xor_ (signed-type   4) (signed-type   8)) (signed-type   8))
(test-return-type     (_xor_ (signed-type   8) (signed-type   4)) (signed-type   8))
(test-return-type     (_xor_ (signed-type   8) (signed-type   8)) (signed-type   8))
(test-return-type     (_xor_ (signed-type   4) (unsigned-type 8)) (signed-type   9))
(test-return-type     (_xor_ (unsigned-type 4) (signed-type   8)) (signed-type   8))
(test-return-type     (_xor_ (signed-type   8) (unsigned-type 4)) (signed-type   8))
(test-return-type     (_xor_ (unsigned-type 8) (signed-type   4)) (signed-type   9))
(test-return-type     (_xor_ (unsigned-type 8) (signed-type   8)) (signed-type   9))
(test-return-type     (_xor_ (signed-type   8) (unsigned-type 8)) (signed-type   9))
(test-return-type/exn (_xor_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_xor_ (unsigned-type 8) (symbol-type 'X)))

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

(test-return-type/exn (unsigned_width (symbol-type 'X)))

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

(test-return-type/exn (signed_width (symbol-type 'X)))

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

(test-return-type (_==_ (any-type) (any-type)) (unsigned-type 1))

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

(test-return-type (_/=_ (any-type) (any-type)) (unsigned-type 1))

; ------------------------------------------------------------------------------
; _>_
; ------------------------------------------------------------------------------

(test-function (_>_ -5 -5) 0)
(test-function (_>_ -5 10) 0)
(test-function (_>_ 10 -5) 1)
(test-function (_>_ 10 10) 0)

(test-return-type (_>_ (signed-type 4) (unsigned-type 5)) (unsigned-type 1))

(test-return-type/exn (_>_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_>_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _<_
; ------------------------------------------------------------------------------

(test-function (_<_ -5 -5) 0)
(test-function (_<_ -5 10) 1)
(test-function (_<_ 10 -5) 0)
(test-function (_<_ 10 10) 0)

(test-return-type (_<_ (signed-type 4) (unsigned-type 5)) (unsigned-type 1))

(test-return-type/exn (_<_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_<_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _>=_
; ------------------------------------------------------------------------------

(test-function (_>=_ -5 -5) 1)
(test-function (_>=_ -5 10) 0)
(test-function (_>=_ 10 -5) 1)
(test-function (_>=_ 10 10) 1)

(test-return-type (_>=_ (signed-type 4) (unsigned-type 5)) (unsigned-type 1))

(test-return-type/exn (_>=_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_>=_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _<=_
; ------------------------------------------------------------------------------

(test-function (_<=_ -5 -5) 1)
(test-function (_<=_ -5 10) 1)
(test-function (_<=_ 10 -5) 0)
(test-function (_<=_ 10 10) 1)

(test-return-type (_<=_ (signed-type 4) (unsigned-type 5)) (unsigned-type 1))

(test-return-type/exn (_<=_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_<=_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _+_
; ------------------------------------------------------------------------------

(test-function (_+_ 10  200)  210)
(test-function (_+_ 10 -200) -190)

(test-return-type (_+_ (unsigned-type 4) (unsigned-type 4)) (unsigned-type 5))
(test-return-type (_+_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 9))
(test-return-type (_+_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type 9))
(test-return-type (_+_ (signed-type   4) (signed-type   4)) (signed-type   5))
(test-return-type (_+_ (signed-type   4) (signed-type   8)) (signed-type   9))
(test-return-type (_+_ (signed-type   8) (signed-type   4)) (signed-type   9))
(test-return-type (_+_ (signed-type   4) (unsigned-type 4)) (signed-type   6))
(test-return-type (_+_ (signed-type   4) (unsigned-type 8)) (signed-type  10))
(test-return-type (_+_ (signed-type   8) (unsigned-type 4)) (signed-type   9))
(test-return-type (_+_ (unsigned-type 4) (signed-type   4)) (signed-type   6))
(test-return-type (_+_ (unsigned-type 4) (signed-type   8)) (signed-type   9))
(test-return-type (_+_ (unsigned-type 8) (signed-type   4)) (signed-type  10))

(test-return-type/exn (_+_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_+_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _-_
; ------------------------------------------------------------------------------

(test-function (_-_ 10  200) -190)
(test-function (_-_ 10 -200)  210)

(test-return-type (_-_ (unsigned-type 4) (unsigned-type 4)) (unsigned-type 5))
(test-return-type (_-_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 9))
(test-return-type (_-_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type 9))
(test-return-type (_-_ (signed-type   4) (signed-type   4)) (signed-type   5))
(test-return-type (_-_ (signed-type   4) (signed-type   8)) (signed-type   9))
(test-return-type (_-_ (signed-type   8) (signed-type   4)) (signed-type   9))
(test-return-type (_-_ (signed-type   4) (unsigned-type 4)) (signed-type   6))
(test-return-type (_-_ (signed-type   4) (unsigned-type 8)) (signed-type  10))
(test-return-type (_-_ (signed-type   8) (unsigned-type 4)) (signed-type   9))
(test-return-type (_-_ (unsigned-type 4) (signed-type   4)) (signed-type   6))
(test-return-type (_-_ (unsigned-type 4) (signed-type   8)) (signed-type   9))
(test-return-type (_-_ (unsigned-type 8) (signed-type   4)) (signed-type  10))

(test-return-type/exn (_-_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_-_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _*_
; ------------------------------------------------------------------------------

(test-function (_*_ 10  200)  2000)
(test-function (_*_ 10 -200) -2000)

(test-return-type (_*_ (unsigned-type 4) (unsigned-type 4)) (unsigned-type  8))
(test-return-type (_*_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 12))
(test-return-type (_*_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type 12))
(test-return-type (_*_ (signed-type   4) (signed-type   4)) (signed-type    8))
(test-return-type (_*_ (signed-type   4) (signed-type   8)) (signed-type   12))
(test-return-type (_*_ (signed-type   8) (signed-type   4)) (signed-type   12))
(test-return-type (_*_ (signed-type   4) (unsigned-type 4)) (signed-type    8))
(test-return-type (_*_ (signed-type   4) (unsigned-type 8)) (signed-type   12))
(test-return-type (_*_ (signed-type   8) (unsigned-type 4)) (signed-type   12))
(test-return-type (_*_ (unsigned-type 4) (signed-type   4)) (signed-type    8))
(test-return-type (_*_ (unsigned-type 4) (signed-type   8)) (signed-type   12))
(test-return-type (_*_ (unsigned-type 8) (signed-type   4)) (signed-type   12))

(test-return-type/exn (_*_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_*_ (unsigned-type 8) (symbol-type 'X)))

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

(test-return-type (_/_ (unsigned-type 4) (unsigned-type 4)) (unsigned-type  4))
(test-return-type (_/_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type  4))
(test-return-type (_/_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type  8))
(test-return-type (_/_ (signed-type   4) (signed-type   4)) (signed-type    5))
(test-return-type (_/_ (signed-type   4) (signed-type   8)) (signed-type    5))
(test-return-type (_/_ (signed-type   8) (signed-type   4)) (signed-type    9))
(test-return-type (_/_ (signed-type   4) (unsigned-type 4)) (signed-type    4))
(test-return-type (_/_ (signed-type   4) (unsigned-type 8)) (signed-type    4))
(test-return-type (_/_ (signed-type   8) (unsigned-type 4)) (signed-type    8))
(test-return-type (_/_ (unsigned-type 4) (signed-type   4)) (signed-type    5))
(test-return-type (_/_ (unsigned-type 4) (signed-type   8)) (signed-type    5))
(test-return-type (_/_ (unsigned-type 8) (signed-type   4)) (signed-type    9))

(test-return-type/exn (_/_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_/_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _neg_
; ------------------------------------------------------------------------------

(test-function (_neg_   0)   0)
(test-function (_neg_  10) -10)
(test-function (_neg_ -10)  10)

(test-return-type (_neg_ (unsigned-type 4)) (signed-type  5))
(test-return-type (_neg_ (signed-type   4)) (signed-type  5))

(test-return-type/exn (_neg_ (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _<<_
; ------------------------------------------------------------------------------

(test-function (_<<_  11  0)   11)
(test-function (_<<_  11  4)  176)
(test-function (_<<_  11 -2)    2)
(test-function (_<<_ -11  0)  -11)
(test-function (_<<_ -11  4) -176)
(test-function (_<<_ -11 -2)   -3)

(test-return-type (_<<_ (unsigned-type 4) (unsigned-type 4)) (unsigned-type  19))
(test-return-type (_<<_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type 259))
(test-return-type (_<<_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type  23))
(test-return-type (_<<_ (signed-type   4) (signed-type   4)) (signed-type    11))
(test-return-type (_<<_ (signed-type   4) (signed-type   8)) (signed-type   131))
(test-return-type (_<<_ (signed-type   8) (signed-type   4)) (signed-type    15))
(test-return-type (_<<_ (signed-type   4) (unsigned-type 4)) (signed-type    19))
(test-return-type (_<<_ (signed-type   4) (unsigned-type 8)) (signed-type   259))
(test-return-type (_<<_ (signed-type   8) (unsigned-type 4)) (signed-type    23))
(test-return-type (_<<_ (unsigned-type 4) (signed-type   4)) (unsigned-type  11))
(test-return-type (_<<_ (unsigned-type 4) (signed-type   8)) (unsigned-type 131))
(test-return-type (_<<_ (unsigned-type 8) (signed-type   4)) (unsigned-type  15))

(test-return-type/exn (_<<_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_<<_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _>>_
; ------------------------------------------------------------------------------

(test-function (_>>_  177  0)   177)
(test-function (_>>_  177  4)    11)
(test-function (_>>_  177 -2)   708)
(test-function (_>>_ -177  0)  -177)
(test-function (_>>_ -177  4)   -12)
(test-function (_>>_ -177 -2)  -708)

(test-return-type (_>>_ (unsigned-type 4) (unsigned-type 4)) (unsigned-type   4))
(test-return-type (_>>_ (unsigned-type 4) (unsigned-type 8)) (unsigned-type   4))
(test-return-type (_>>_ (unsigned-type 8) (unsigned-type 4)) (unsigned-type   8))
(test-return-type (_>>_ (signed-type   4) (signed-type   4)) (signed-type    12))
(test-return-type (_>>_ (signed-type   4) (signed-type   8)) (signed-type   132))
(test-return-type (_>>_ (signed-type   8) (signed-type   4)) (signed-type    16))
(test-return-type (_>>_ (signed-type   4) (unsigned-type 4)) (signed-type     4))
(test-return-type (_>>_ (signed-type   4) (unsigned-type 8)) (signed-type     4))
(test-return-type (_>>_ (signed-type   8) (unsigned-type 4)) (signed-type     8))
(test-return-type (_>>_ (unsigned-type 4) (signed-type   4)) (unsigned-type  12))
(test-return-type (_>>_ (unsigned-type 4) (signed-type   8)) (unsigned-type 132))
(test-return-type (_>>_ (unsigned-type 8) (signed-type   4)) (unsigned-type  16))

(test-return-type/exn (_>>_ (symbol-type 'X)  (unsigned-type 8)))
(test-return-type/exn (_>>_ (unsigned-type 8) (symbol-type 'X)))

; ------------------------------------------------------------------------------
; _slice_
; ------------------------------------------------------------------------------

(test-function (_slice_  #xAB03 11 8) #xB)
(test-function (_slice_  #xAB03 12 8) #xB)
(test-function (_slice_ #x-AB03 11 8) #x4)
(test-function (_slice_ #x-AB03 12 8) #x-C)

(test-return-type (_slice_ (unsigned-type 16) (const-type/literal 11) (const-type/literal 8)) (unsigned-type 4))
(test-return-type (_slice_ (signed-type   16) (const-type/literal 11) (const-type/literal 8)) (signed-type 4))

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

(test-return-type (_set_slice_ (unsigned-type 16) (const-type/literal 11) (const-type/literal 8) (const-type/literal 4)) (unsigned-type 16))
(test-return-type (_set_slice_ (signed-type 16) (const-type/literal 11) (const-type/literal 8) (const-type/literal 4)) (signed-type 16))

(test-return-type/exn (_set_slice_ (any-type) (const-type/literal 11) (const-type/literal 8) (const-type/literal 4)))
(test-return-type/exn (_set_slice_ (signed-type 16) (any-type) (const-type/literal 8) (const-type/literal 4)))
(test-return-type/exn (_set_slice_ (signed-type 16) (const-type/literal 11) (any-type) (const-type/literal 4)))
(test-return-type/exn (_set_slice_ (signed-type 16) (const-type/literal 11) (const-type/literal 8) (any-type)))

; ------------------------------------------------------------------------------
; _concat_
; ------------------------------------------------------------------------------

(test-function (_concat_) 0)
(test-function (_concat_ #xA (unsigned-type 4) #xB (unsigned-type 8) #xFC (unsigned-type 8)) #xA0BFC)
(test-function (_concat_ #xA (signed-type   4) #xB (unsigned-type 8) #xFC (unsigned-type 8)) #x-5F404)
(test-function (_concat_ #xA (unsigned-type 4) #xB (signed-type   8) #xFC (signed-type 8))   #xA0BFC)
(test-function (_concat_ #xA (signed-type   4) #xB (signed-type   8) #xFC (signed-type 8))   #x-5F404)

(test-return-type (_concat_) (const-type 0 (unsigned-type 1)))

(test-return-type (_concat_ (unsigned-type 1) (const-type/literal (unsigned-type 4))
                            (unsigned-type 2) (const-type/literal (unsigned-type 5))
                            (unsigned-type 3) (const-type/literal (unsigned-type 6)))
                  (unsigned-type 15))

(test-return-type (_concat_ (unsigned-type 1) (const-type/literal (signed-type   4))
                            (unsigned-type 2) (const-type/literal (unsigned-type 5))
                            (unsigned-type 3) (const-type/literal (unsigned-type 6)))
                  (signed-type 15))

(test-return-type (_concat_ (unsigned-type 1) (const-type/literal (unsigned-type 4))
                            (unsigned-type 2) (const-type/literal (signed-type   5))
                            (unsigned-type 3) (const-type/literal (signed-type   6)))
                  (unsigned-type 15))

(test-return-type (_concat_ (unsigned-type 1) (const-type/literal (signed-type 4))
                            (unsigned-type 2) (const-type/literal (signed-type 5))
                            (unsigned-type 3) (const-type/literal (signed-type 6)))
                  (signed-type 15))

; ------------------------------------------------------------------------------
; _array_
; ------------------------------------------------------------------------------

(test-function (_array_) (pvector))
(test-function (_array_ 10) (pvector 10))
(test-function (_array_ 10 20 30) (pvector 10 20 30))

(test-return-type (_array_ (unsigned-type 1) (unsigned-type 4) (unsigned-type 8))
                  (array-type 3 (union-type (list (unsigned-type 1) (unsigned-type 4) (unsigned-type 8)))))

; ------------------------------------------------------------------------------
; _record_
; ------------------------------------------------------------------------------

(test-function (_record_ 'x 12 'y 50) #hash((x . 12) (y . 50)))

(test-return-type (_record_ (const-type/literal 'x) (unsigned-type 4) (const-type/literal 'y) (unsigned-type 6))
                  (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 6))))

(test-return-type/exn (_record_ (unsigned-type 4) (unsigned-type 8)))

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

(test-return-type (_nth_ (array-type 3 (unsigned-type 4)) (unsigned-type 2)) (unsigned-type 4))
(test-return-type/exn (_nth_ (array-type 3 (unsigned-type 4)) (symbol-type 'x)))

(test-return-type (_nth_ (array:impl 2 3 (unsigned-type 4)) (unsigned-type 2)) (array-type 3 (unsigned-type 4)))
(test-return-type (_nth_ (array:impl 2 3 (unsigned-type 4)) (unsigned-type 2) (unsigned-type 2)) (unsigned-type 4))

(test-return-type/exn (_nth_ (unsigned-type 8) (unsigned-type 4)))
(test-return-type/exn (_nth_ (unsigned-type 8) (symbol-type 'x)))

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

(test-return-type (_set_nth_ (array-type 3 (unsigned-type 4)) (unsigned-type 2) (unsigned-type 3)) (array-type 3 (unsigned-type 4)))
(test-return-type (_set_nth_ (array:impl 2 3 (unsigned-type 4)) (tuple-type (list (unsigned-type 2) (unsigned-type 2))) (unsigned-type 3)) (array:impl 2 3 (unsigned-type 4)))
(test-return-type/exn (_set_nth_ (array-type 3 (unsigned-type 4)) (unsigned-type 2) (unsigned-type 5)))
(test-return-type/exn (_set_nth_ (array-type 3 (unsigned-type 4)) (unsigned-type 2) (symbol-type 'x)))
(test-return-type/exn (_set_nth_ (array-type 3 (unsigned-type 4)) (symbol-type 'x) (unsigned-type 3)))

; TODO multidimensional arrays

; ------------------------------------------------------------------------------
; _field_
; ------------------------------------------------------------------------------

(test-function (_field_ (hash 'x 10 'y 20) 'x) 10)
(test-function (_field_ (hash 'x 10 'y 20) 'y) 20)
(test-function/exn (_field_ (hash 'x 10 'y 20) 'z))

(test-return-type (_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8))) (const-type/literal 'x)) (unsigned-type 4))
(test-return-type (_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8))) (const-type/literal 'y)) (unsigned-type 8))
(test-return-type/exn (_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8))) (const-type/literal 'z)))
(test-return-type/exn (_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8))) (const-type/literal 12)))
(test-return-type/exn (_field_ (any-type) (const-type/literal 'z)))

; ------------------------------------------------------------------------------
; _set_field_
; ------------------------------------------------------------------------------

(test-function (_set_field_ (hash 'x 10 'y 20) 'x 55) (hash 'x 55 'y 20))
(test-function (_set_field_ (hash 'x 10 'y 20) 'y 55) (hash 'x 10 'y 55))
(test-function (_set_field_ (hash 'x 10 'y 20 'z 30) 'x 55 'z 66) (hash 'x 55 'y 20 'z 66))

(test-return-type (_set_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8)))
                               (const-type/literal 'x) (unsigned-type 3))
                  (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8))))
(test-return-type (_set_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8)))
                               (const-type/literal 'y) (unsigned-type 7))
                  (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8))))
(test-return-type/exn (_set_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8)))
                                   (const-type/literal 'z) (unsigned-type 3)))
(test-return-type/exn (_set_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8)))
                                   (const-type/literal 'x) (unsigned-type 5)))
(test-return-type/exn (_set_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8)))
                                   (const-type/literal 'y) (unsigned-type 9)))
(test-return-type/exn (_set_field_ (record-type (hash 'x (unsigned-type 4) 'y (unsigned-type 8)))
                                   (const-type/literal 12) (unsigned-type 3)))
(test-return-type/exn (_set_field_ (any-type)
                                   (const-type/literal 'x) (unsigned-type 3)))

; ------------------------------------------------------------------------------
; _cast_
; ------------------------------------------------------------------------------

(test-function (_cast_ (unsigned-type 8) 36) 36)
(test-function (_cast_ (unsigned-type 4) 36) 4)
(test-function (_cast_ (unsigned-type 8) -36) 220)
(test-function (_cast_ (signed-type 8) 36) 36)
(test-function (_cast_ (signed-type 3) 36) -4)
(test-function (_cast_ (signed-type 8) -36) -36)
(test-function (_cast_ (any-type) 'x) 'x)
(test-function/exn (_cast_ (signed-type 8) 'x))

(test-function (_cast_ (signed-type #f) 36) -28)
(test-function (_cast_ (signed-type #f) -36) -36)
(test-function (_cast_ (unsigned-type #f) 36) 36)
(test-function (_cast_ (unsigned-type #f) -36) 92)

(test-return-type (_cast_ (const-type/literal (unsigned-type 8)) (unsigned-type 5)) (unsigned-type 8))
(test-return-type (_cast_ (const-type/literal (unsigned-type 4)) (unsigned-type 5)) (unsigned-type 4))
(test-return-type (_cast_ (const-type/literal (unsigned-type 8)) (signed-type 5)) (unsigned-type 8))
(test-return-type (_cast_ (const-type/literal (signed-type 8)) (unsigned-type 5)) (signed-type 8))
(test-return-type (_cast_ (const-type/literal (signed-type 3)) (unsigned-type 5)) (signed-type 3))
(test-return-type (_cast_ (const-type/literal (signed-type 8)) (signed-type 5)) (signed-type 8))
(test-return-type (_cast_ (const-type/literal (unsigned-type #f)) (unsigned-type 5)) (unsigned-type 5))
(test-return-type (_cast_ (const-type/literal (unsigned-type #f)) (signed-type 5)) (unsigned-type 5))
(test-return-type (_cast_ (const-type/literal (signed-type #f)) (unsigned-type 5)) (signed-type 5))
(test-return-type (_cast_ (const-type/literal (signed-type #f)) (signed-type 5)) (signed-type 5))

(test-return-type/exn (_cast_ (const-type/literal 40) (unsigned-type 5)))
(test-return-type/exn (_cast_ (const-type/literal (unsigned-type #f)) (any-type)))
(test-return-type/exn (_cast_ (const-type/literal (signed-type #f)) (any-type)))

; ------------------------------------------------------------------------------
; _range_
; ------------------------------------------------------------------------------

(test-function (_range_ 0 3) (range 4))
(test-function (_range_ -3 3) (range -3 4))
(test-function (_range_ 3 0) (range 3 -1 -1))
(test-function (_range_ 3 -3) (range 3 -4 -1))

(test-return-type (_range_ (unsigned-type 4) (unsigned-type 8)) (range-type (unsigned-type 8)))
(test-return-type (_range_ (signed-type   4) (unsigned-type 8)) (range-type (signed-type   9)))
(test-return-type (_range_ (signed-type   4) (signed-type   8)) (range-type (signed-type   8)))

(test-return-type/exn (_range_ (any-type)        (unsigned-type 8)))
(test-return-type/exn (_range_ (unsigned-type 8) (any-type)))
(test-return-type/exn (_range_ (any-type)        (any-type)))

; ------------------------------------------------------------------------------
; zero
; ------------------------------------------------------------------------------

(test-function (zero (signed-type 8))   0)
(test-function (zero (unsigned-type 8)) 0)
(test-function (zero (array-type 3 (unsigned-type 8))) (make-pvector 3 0))
(test-function (zero (tuple-type (list (unsigned-type 8) (array-type 3 (unsigned-type 8)))))
               (list 0 (make-pvector 3 0)))
(test-function (zero (record-type (hash 'x (unsigned-type 8) 'y (array-type 3 (unsigned-type 8)))))
               (hash 'x 0 'y (make-pvector 3 0)))
(test-function (zero (union-type (list (symbol-type 'a) (symbol-type 'b) (symbol-type 'c))))
               'a)
(test-function (zero (union-type (list
                                   (union-type (list (symbol-type 'a) (symbol-type 'b)))
                                   (symbol-type 'c))))
               'a)
(test-function (zero (union-type (list
                                   (symbol-type 'a)
                                   (union-type (list (symbol-type 'b) (symbol-type 'c))))))
               'a)
(test-function (zero (union-type (list
                                   (union-type (list (symbol-type 'a) (symbol-type 'b)))
                                   (union-type (list (symbol-type 'c) (symbol-type 'd))))))
               'a)
