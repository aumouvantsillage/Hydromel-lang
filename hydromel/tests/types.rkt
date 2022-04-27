; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  "../lib/types.rkt"
  "../lib/std.rkt")

(test-true  "integer <: any"                (<: (integer)      (any)))
(test-true  "natural <: integer"            (<: (natural)      (integer)))

(test-true  "signed(32) <: integer"         (<: (signed 32)    (integer)))
(test-true  "signed(31) <: signed(32)"      (<: (signed 31)    (signed 32)))
(test-false "signed(32) /<: signed(31)"     (<: (signed 32)    (signed 31)))

(test-true  "unsigned(32) <: natural"       (<: (unsigned 32)  (natural)))
(test-true  "unsigned(31) <: unsigned(32)"  (<: (unsigned 31)  (unsigned 32)))
(test-false "unsigned(32) /<: unsigned(31)" (<: (unsigned 32)  (unsigned 31)))

(test-true  "unsigned(31) <: signed(32)"    (<: (unsigned 31)  (signed 32)))
(test-false "unsigned(32) /<: signed(32)"   (<: (unsigned 32)  (signed 32)))
(test-false "signed(32) /<: unsigned(32)"   (<: (signed 32)    (unsigned 32)))
(test-false "signed(1) /<: unsigned(32)"    (<: (signed 1)     (unsigned 32)))

(test-true  "15 : unsigned(4)"              (<: (type-of 15)   (unsigned 4)))
(test-true  "0 : unsigned(4)"               (<: (type-of 0)    (unsigned 4)))
(test-false "-1 /: unsigned(4)"             (<: (type-of -1)   (unsigned 4)))
(test-false "16 /: unsigned(4)"             (<: (type-of 16)   (unsigned 4)))

(test-true  "-8 : signed(4)"                (<: (type-of -8)   (signed 4)))
(test-true  "7 : signed(4)"                 (<: (type-of 7)    (signed 4)))
(test-false "8 : signed(4)"                 (<: (type-of 8)    (signed 4)))
(test-false "-9 : signed(4)"                (<: (type-of -9)   (signed 4)))

(test-true "~a : symbol(~a)"                (<: (type-of '~a)  (symbol-type '~a)))
(test-true "~a : symbol()"                  (<: (type-of '~a)  (symbol-type #f)))

(test-true  "array(4, unsigned(8)) <: array(3, unsigned 8)"  (<: (array 4 (unsigned 8)) (array 3 (unsigned 8))))
(test-false "array(4, unsigned(8)) /<: array(5, unsigned 8)" (<: (array 4 (unsigned 8)) (array 5 (unsigned 8))))
(test-true  "array(4, unsigned(8)) <: array(4, unsigned 9)"  (<: (array 4 (unsigned 8)) (array 4 (unsigned 9))))
(test-false "array(4, unsigned(8)) /<: array(4, unsigned 7)" (<: (array 4 (unsigned 8)) (array 4 (unsigned 7))))
(test-true  "array(4, unsigned(8)) <: array(3, unsigned 9)"  (<: (array 4 (unsigned 8)) (array 3 (unsigned 9))))
(test-false "array(4, unsigned(8)) /<: array(5, unsigned 7)" (<: (array 4 (unsigned 8)) (array 5 (unsigned 7))))

(test-true "tuple(unsigned(16), array(4, unsigned 8)) <: tuple(unsigned(32), array(3, unsigned(9)))"
           (<: (tuple (unsigned 16) (array 4 (unsigned 8)))
               (tuple (unsigned 32) (array 3 (unsigned 9)))))

(test-true "tuple(unsigned(16), array(4, unsigned 8), symbol(~x)) <: tuple(unsigned(16), array(4, unsigned(8)))"
           (<: (tuple (unsigned 16) (array 4 (unsigned 8)) (symbol-type '~x))
               (tuple (unsigned 16) (array 4 (unsigned 8)))))

; TODO const-type
; TODO record
; TODO union
; TODO range? should be an array or tuple?
