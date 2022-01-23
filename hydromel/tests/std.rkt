; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  syntax/parse/define
  "../lib/std.rkt"
  "../lib/expander.rkt"
  (prefix-in t/ "../lib/types.rkt")
  (for-syntax
    racket/match
    racket/syntax
    (prefix-in meta/ "../lib/meta.rkt")))

(define-syntax-parser call
  [(_ name arg ...)
   (match-define (struct meta/function (fn-name cast?)) (syntax-local-value #'name))
   (if cast?
     #`(let ([rt (return-type name (t/literal-type arg) ...)])
         (rt (#,fn-name arg ...)))
     #`(#,fn-name arg ...))])

(define-syntax-parse-rule (return-type name arg ...)
   #:with fn-name (meta/function-name (syntax-local-value #'name))
   #:with rt-name (format-id #'fn-name "~a:return-type" #'fn-name)
   (rt-name arg ...))

; ------------------------------------------------------------------------------
; int->bool
; ------------------------------------------------------------------------------

(test-equal? "int->bool(0)" (call int->bool 0) #f)
(test-equal? "int->bool(1)" (call int->bool 1) #t)
(test-equal? "int->bool(2)" (call int->bool 2) #t)

(test-equal? "int->bool:return-type(integer)" (return-type int->bool (t/abstract-integer #f)) (t/boolean))
(test-exn    "int->bool:return-type(symbol)" exn:fail? (thunk (return-type int->bool (t/symbol 'X))))

; ------------------------------------------------------------------------------
; TODO _if_
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; TODO _case_
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; _not_
; ------------------------------------------------------------------------------

(test-equal? "not(0)"  (call _not_  0) 1)
(test-equal? "not(1)"  (call _not_  1) 0)
(test-equal? "not(2)"  (call _not_  2) 1)
(test-equal? "not(3)"  (call _not_  3) 0)
(test-equal? "not(4)"  (call _not_  4) 3)
(test-equal? "not(5)"  (call _not_  5) 2)
(test-equal? "not(-1)" (call _not_ -1) 0)
(test-equal? "not(-2)" (call _not_ -2) 1)
(test-equal? "not(-3)" (call _not_ -3) 2)
(test-equal? "not(-4)" (call _not_ -4) 3)

(test-equal? "not:return-type(unsigned(1))" (return-type _not_ (t/unsigned 1)) (t/unsigned 1))
(test-equal? "not:return-type(unsigned(2))" (return-type _not_ (t/unsigned 2)) (t/unsigned 2))
(test-equal? "not:return-type(signed(1))"   (return-type _not_ (t/signed 1))   (t/signed 1))
(test-equal? "not:return-type(signed(2))"   (return-type _not_ (t/signed 2))   (t/signed 2))
(test-exn    "not:return-type(symbol)" exn:fail? (thunk (return-type _not_ (t/symbol 'X))))

; ------------------------------------------------------------------------------
; _and_
; ------------------------------------------------------------------------------

(test-equal? "and(0, 0)"    (call _and_    0     0)    0)
(test-equal? "and(0, 1)"    (call _and_    0     1)    0)
(test-equal? "and(1, 0)"    (call _and_    1     0)    0)
(test-equal? "and(1, 1)"    (call _and_    1     1)    1)
(test-equal? "and(F0, 3C)"  (call _and_ #xF0  #x3C) #x30)
(test-equal? "and(-1, 12)"  (call _and_   -1    12)   12)
(test-equal? "and(-24, 12)" (call _and_  -24    12)    8)
(test-equal? "and(-24, -4)" (call _and_  -24    -4)  -24)

(test-equal? "and:return-type(unsigned(4), unsigned(8))" (return-type _and_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "and:return-type(unsigned(8), unsigned(4))" (return-type _and_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 8))
(test-equal? "and:return-type(unsigned(8), unsigned(8))" (return-type _and_ (t/unsigned 8) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "and:return-type(signed(4), signed(8))"     (return-type _and_ (t/signed   4) (t/signed   8)) (t/signed   8))
(test-equal? "and:return-type(signed(8), signed(4))"     (return-type _and_ (t/signed   8) (t/signed   4)) (t/signed   8))
(test-equal? "and:return-type(signed(8), signed(8))"     (return-type _and_ (t/signed   8) (t/signed   8)) (t/signed   8))
(test-equal? "and:return-type(signed(4), unsigned(8))"   (return-type _and_ (t/signed   4) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "and:return-type(unsigned(4), signed(8))"   (return-type _and_ (t/unsigned 4) (t/signed   8)) (t/unsigned 8))
(test-equal? "and:return-type(signed(8), unsigned(4))"   (return-type _and_ (t/signed   8) (t/unsigned 4)) (t/unsigned 8))
(test-equal? "and:return-type(unsigned(8), signed(4))"   (return-type _and_ (t/unsigned 8) (t/signed   4)) (t/unsigned 8))
(test-equal? "and:return-type(unsigned(8), signed(8))"   (return-type _and_ (t/unsigned 8) (t/signed   8)) (t/unsigned 8))
(test-equal? "and:return-type(signed(8), unsigned(8))"   (return-type _and_ (t/signed   8) (t/unsigned 8)) (t/unsigned 8))
(test-exn    "and:return-type(symbol, unsigned(8))" exn:fail? (thunk (return-type _and_ (t/symbol 'X)  (t/unsigned 8))))
(test-exn    "and:return-type(unsigned(8), symbol)" exn:fail? (thunk (return-type _and_ (t/unsigned 8) (t/symbol 'X))))

; ------------------------------------------------------------------------------
; _or_
; ------------------------------------------------------------------------------

(test-equal? "or(0, 0)"    (call _or_    0    0)    0)
(test-equal? "or(0, 1)"    (call _or_    0    1)    1)
(test-equal? "or(1, 0)"    (call _or_    1    0)    1)
(test-equal? "or(1, 1)"    (call _or_    1    1)    1)
(test-equal? "or(F0, 3C)"  (call _or_ #xF0 #x3C) #xFC)
(test-equal? "or(-1, 12)"  (call _or_   -1   12)   -1)
(test-equal? "or(-24, 12)" (call _or_  -24   12)  -20)
(test-equal? "or(-24, -4)" (call _or_  -24   -4)   -4)

(test-equal? "or:return-type(unsigned(4), unsigned(8))" (return-type _or_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "or:return-type(unsigned(8), unsigned(4))" (return-type _or_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 8))
(test-equal? "or:return-type(unsigned(8), unsigned(8))" (return-type _or_ (t/unsigned 8) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "or:return-type(signed(4), signed(8))"     (return-type _or_ (t/signed   4) (t/signed   8)) (t/signed   8))
(test-equal? "or:return-type(signed(8), signed(4))"     (return-type _or_ (t/signed   8) (t/signed   4)) (t/signed   8))
(test-equal? "or:return-type(signed(8), signed(8))"     (return-type _or_ (t/signed   8) (t/signed   8)) (t/signed   8))
(test-equal? "or:return-type(signed(4), unsigned(8))"   (return-type _or_ (t/signed   4) (t/unsigned 8)) (t/signed   8))
(test-equal? "or:return-type(unsigned(4), signed(8))"   (return-type _or_ (t/unsigned 4) (t/signed   8)) (t/signed   8))
(test-equal? "or:return-type(signed(8), unsigned(4))"   (return-type _or_ (t/signed   8) (t/unsigned 4)) (t/signed   8))
(test-equal? "or:return-type(unsigned(8), signed(4))"   (return-type _or_ (t/unsigned 8) (t/signed   4)) (t/signed   8))
(test-equal? "or:return-type(unsigned(8), signed(8))"   (return-type _or_ (t/unsigned 8) (t/signed   8)) (t/signed   8))
(test-equal? "or:return-type(signed(8), unsigned(8))"   (return-type _or_ (t/signed   8) (t/unsigned 8)) (t/signed   8))
(test-exn    "or:return-type(symbol, unsigned(8))" exn:fail? (thunk (return-type _or_ (t/symbol 'X)  (t/unsigned 8))))
(test-exn    "or:return-type(unsigned(8), symbol)" exn:fail? (thunk (return-type _or_ (t/unsigned 8) (t/symbol 'X))))

; ------------------------------------------------------------------------------
; _xor_
; ------------------------------------------------------------------------------

(test-equal? "xor(0, 0)"    (call _xor_    0    0)    0)
(test-equal? "xor(0, 1)"    (call _xor_    0    1)    1)
(test-equal? "xor(1, 0)"    (call _xor_    1    0)    1)
(test-equal? "xor(1, 1)"    (call _xor_    1    1)    0)
(test-equal? "xor(F0, 3C)"  (call _xor_ #xF0 #x3C) #xCC)
(test-equal? "xor(-1, 12)"  (call _xor_   -1   12)  -13)
(test-equal? "xor(-24, 12)" (call _xor_  -24   12)  -28)
(test-equal? "xor(-24, -5)" (call _xor_  -24   -4)   20)

(test-equal? "xor:return-type(unsigned(4), unsigned(8))" (return-type _xor_ (t/unsigned 4) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "xor:return-type(unsigned(8), unsigned(4))" (return-type _xor_ (t/unsigned 8) (t/unsigned 4)) (t/unsigned 8))
(test-equal? "xor:return-type(unsigned(8), unsigned(8))" (return-type _xor_ (t/unsigned 8) (t/unsigned 8)) (t/unsigned 8))
(test-equal? "xor:return-type(signed(4), signed(8))"     (return-type _xor_ (t/signed   4) (t/signed   8)) (t/signed   8))
(test-equal? "xor:return-type(signed(8), signed(4))"     (return-type _xor_ (t/signed   8) (t/signed   4)) (t/signed   8))
(test-equal? "xor:return-type(signed(8), signed(8))"     (return-type _xor_ (t/signed   8) (t/signed   8)) (t/signed   8))
(test-equal? "xor:return-type(signed(4), unsigned(8))"   (return-type _xor_ (t/signed   4) (t/unsigned 8)) (t/signed   9))
(test-equal? "xor:return-type(unsigned(4), signed(8))"   (return-type _xor_ (t/unsigned 4) (t/signed   8)) (t/signed   8))
(test-equal? "xor:return-type(signed(8), unsigned(4))"   (return-type _xor_ (t/signed   8) (t/unsigned 4)) (t/signed   8))
(test-equal? "xor:return-type(unsigned(8), signed(4))"   (return-type _xor_ (t/unsigned 8) (t/signed   4)) (t/signed   9))
(test-equal? "xor:return-type(unsigned(8), signed(8))"   (return-type _xor_ (t/unsigned 8) (t/signed   8)) (t/signed   9))
(test-equal? "xor:return-type(signed(8), unsigned(8))"   (return-type _xor_ (t/signed   8) (t/unsigned 8)) (t/signed   9))
(test-exn    "xor:return-type(symbol, unsigned(8))" exn:fail? (thunk (return-type _xor_ (t/symbol 'X)  (t/unsigned 8))))
(test-exn    "xor:return-type(unsigned(8), symbol)" exn:fail? (thunk (return-type _xor_ (t/unsigned 8) (t/symbol 'X))))

; TODO test unsigned_width
; TODO test signed_width
; TODO test _==_
; TODO test _/=_
; TODO test _>_
; TODO test _<_
; TODO test _>=_
; TODO test _<=_
; TODO test _+_
; TODO test _-_
; TODO test _*_
; TODO test _/_
; TODO test _neg_
; TODO test _<<_
; TODO test _>>_
; TODO test _range_
; TODO test _slice_
; TODO test _concat_
; TODO test _array_
; TODO test _record_
; TODO test _nth_
; TODO test _set_nth_
; TODO test _field_
; TODO test _cast_
