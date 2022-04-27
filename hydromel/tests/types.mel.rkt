; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  "types.mel")

(test-not-exn "integer <: any"             test_integer_is_any-make)
(test-exn     "any /<: integer"  exn:fail? test_any_is_not_integer-make)

(test-not-exn "natural <: integer"         test_natural_is_integer-make)

(test-not-exn "signed(32) <: integer"                test_signed32_is_integer-make)
(test-not-exn "signed(31) <: signed(32)"             test_signed31_is_signed32-make)
(test-exn     "signed(32) /<: signed(31)"  exn:fail? test_signed32_is_not_signed31-make)

(test-not-exn "unsigned(32) <: natural"                  test_unsigned32_is_natural-make)
(test-not-exn "unsigned(31) <: unsigned(32)"             test_unsigned31_is_unsigned32-make)
(test-exn     "unsigned(32) /<: unsigned(31)"  exn:fail? test_unsigned32_is_not_unsigned31-make)

(test-not-exn "unsigned(31) <: signed(32)"            test_unsigned31_is_signed32-make)
(test-exn     "unsigned(32) /<: signed(32)" exn:fail? test_unsigned32_is_not_signed32-make)
(test-exn     "signed(32) /<: unsigned(32)" exn:fail? test_signed32_is_not_unsigned32-make)
(test-exn     "signed(1) /<: unsigned(32)"  exn:fail? test_signed1_is_not_unsigned32-make)

(test-not-exn "15 : unsigned(4)"            test_15_is_unsigned4-make)
(test-not-exn "0 : unsigned(4)"             test_0_is_unsigned4-make)
(test-exn     "-1 /: unsigned(4)" exn:fail? test_minus1_is_not_unsigned4-make)
(test-exn     "16 /: unsigned(4)" exn:fail? test_16_is_not_unsigned4-make)

(test-not-exn "-8 : signed(4)"            test_minus8_is_signed4-make)
(test-not-exn "7 : signed(4)"             test_7_is_signed4-make)
(test-exn     "8 /: signed(4)"  exn:fail? test_8_is_not_signed4-make)
(test-exn     "-9 /: signed(4)" exn:fail? test_minus9_is_not_signed4-make)

(test-not-exn "array(4, unsigned(8)) <: array(3, unsigned(8))"            test_array4_is_array3-make)
(test-exn     "array(4, unsigned(8)) /<: array(5, unsigned(8))" exn:fail? test_array4_is_not_array5-make)
(test-not-exn "array(4, unsigned(8)) <: array(4, unsigned(9))"            test_array_of_unsigned8_is_array_of_unsigned9-make)
(test-exn     "array(4, unsigned(8)) /<: array(4, unsigned(7))" exn:fail? test_array_of_unsigned8_is_not_array_of_unsigned7-make)
(test-not-exn "array(4, unsigned(8)) <: array(3, unsigned(9))"            test_array4_of_unsigned8_is_array3_of_unsigned9-make)
(test-exn     "array(4, unsigned(8)) /<: array(5, unsigned(7))" exn:fail? test_array4_of_unsigned8_is_not_array5_of_unsigned7-make)

(test-not-exn "tuple(unsigned(16), array(4, unsigned(8))) <: tuple(unsigned(32), array(3, unsigned(9)))"
              test_tuple_u16_a4u8_is_tuple_u32_a3u9-make)
(test-not-exn "tuple(unsigned(16), array(4, unsigned(8)), record(x : bit)) <: tuple(unsigned(16), array(4, unsigned(8)))"
              test_tuple_u16_a4u8_r_is_tuple_u16_a4u8-make)

; TODO record

(test-not-exn "union(signed(8), unsigned(16), unsigned(32)) <: signed(33)"    test_union_s8_u16_u32_is_signed33-make)
(test-exn     "union(signed(8), unsigned(16), unsigned(32)) /<: unsigned(32)" exn:fail? test_union_s8_u16_u32_is_not_unsigned32-make)
(test-not-exn "unsigned(32) <: union(signed(8), unsigned(16), unsigned(32))"  test_unsigned32_is_union_s8_u16_u32-make)
(test-not-exn "signed(8) <: union(signed(8), unsigned(16), unsigned(32))"     test_signed8_is_union_s8_u16_u32-make)
(test-not-exn "~b : enumeration(~a, ~b, ~c)"                                  test_sym_b_is_enum_a_b_c-make)

(test-not-exn "signed(32) : subtype(integer)" test_signed32_is_subtype_integer-make)
(test-exn     "any /: subtype(integer)" exn:fail? test_any_is_not_subtype_integer-make)
