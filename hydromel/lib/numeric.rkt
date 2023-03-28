; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  threading
  syntax/parse/define)

(provide
  min-unsigned-width min-signed-width
  min-signed-value   max-signed-value
  min-unsigned-value max-unsigned-value
  unsigned unsigned-slice set-slice
  unsigned-concat unsigned-concat*
  signed   signed-slice   signed-concat   signed-concat*
  integer->bit-string)

; Returns the minimum bit width to store the integer `val`
; as an unsigned integer value. If `val` ≤ 0, return the
; minimum width of `val` as a signed integer.
(define (min-unsigned-width val)
  (if (positive? val)
    (integer-length   val)
    (min-signed-width val)))

; Returns the minimum bit width to store the integer `val`
; as a signed integer value.
(define (min-signed-width val)
  (add1 (integer-length val)))

; Returns the minimum unsigned value with `width` bits.
(define (min-unsigned-value width)
  0)

; Returns the maximum unsigned value with `width` bits.
(define (max-unsigned-value width)
  (sub1 (arithmetic-shift 1 width)))

; Returns the minimum signed value with `width` bits.
(define (min-signed-value width)
  (arithmetic-shift -1 (sub1 width)))

; Returns the maximum signed value with `width` bits.
(define (max-signed-value width)
  (sub1 (arithmetic-shift 1 (sub1 width))))

; Returns a slice of a value.
; `left` is the index of the most significant bit to keep in the result.
; `right` is the index of the least significant bit to keep in the result.
; This function does not sign-extend the result.
(define (unsigned-slice val left [right left])
  (bitwise-bit-field val right (add1 left)))

; Returns a value with the `width` rightmost bits of `val`.
; This function does not sign-extend the result.
(define (unsigned val width)
  (unsigned-slice val (sub1 width) 0))

; Returns a sign-extended slice of a value.
; `left` is the index of the most significant bit to keep in the result.
; `right` is the index of the least significant bit to keep in the result.
(define (signed-slice val left [right left])
  (define uslice (unsigned-slice val left right))
  (if (bitwise-bit-set? val left)
    (bitwise-ior uslice (arithmetic-shift -1 (- left right)))
    uslice))

; Returns a value with the `width` rightmost bits of `val`.
; This function sign-extends the result.
(define (signed val width)
  (signed-slice val (sub1 width) 0))

; Returns a value with a modified slice.
; `left` is the index of the most significant bit to change.
; `right` is the index of the least significant bit to change.
; `subst` is the value to copy into the specified slice of `val`.
; If the slice width is `width`, only the `width` least significant bits of `subst` are copied.
(define (set-slice val left right subst)
  (define mask (bitwise-ior (arithmetic-shift -1 (add1 left))
                            (sub1 (arithmetic-shift 1 right))))
  (bitwise-ior (bitwise-and val mask)
               (bitwise-and (arithmetic-shift subst right) (bitwise-not mask))))

; Common implementation of unsigned- and signed-concat* functions.
(define (concat init-val items)
  (for/fold ([res init-val])
            ([it  (in-list items)])
    (match-define (list val left right) it)
    (~> res
        (arithmetic-shift (add1 (- left right)))
        (bitwise-ior (unsigned-slice val left right)))))

; Concatenate a list of slices.
; Do not sign-extend the result.
(define (unsigned-concat* items)
  (concat 0 items))

; Helper macro to concatenate a sequence of slices.
; Do not sign-extend the result.
(define-syntax-parse-rule (unsigned-concat [val left right] ...)
  (unsigned-concat* (list [list val left right] ...)))

; Concatenate a sequence of slices.
; Sign-extend the result.
(define (signed-concat* items)
  (match-define (list val left right) (first items))
  (concat (signed-slice val left right) (rest items)))

; Helper macro to concatenate a sequence of slices into a sign-extended value.
(define-syntax-parse-rule (signed-concat [val left right] ...)
  (signed-concat* (list [list val left right] ...)))

; Convert an integer into a binary string of the given width.
(define (integer->bit-string val width)
  (list->string (for/list ([idx (in-range (sub1 width) -1 -1)])
                  (if (bitwise-bit-set? val idx) #\1 #\0))))
