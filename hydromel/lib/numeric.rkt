; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  (for-syntax racket/syntax))

(provide
  min-unsigned-width min-signed-width
  min-signed-value   max-signed-value
  min-unsigned-value max-unsigned-value
  unsigned unsigned-slice unsigned-concat
  signed   signed-slice   signed-concat
  integer->bit-string)

; Returns the minimum bit width to store the integer `v`
; as an unsigned integer value.
(define (min-unsigned-width v)
  (cond [(positive? v) (integer-length v)]
        [else          (min-signed-width v)]))

; Returns the minimum bit width to store the integer `v`
; as an signed integer value.
(define (min-signed-width v)
  (add1 (integer-length v)))

(define (min-unsigned-value w)
  0)

(define (max-unsigned-value w)
  (sub1 (arithmetic-shift 1 w)))

(define (min-signed-value w)
  (arithmetic-shift -1 (sub1 w)))

(define (max-signed-value w)
  (sub1 (arithmetic-shift 1 (sub1 w))))

; Returns a slice of a value.
; left is the index of the most significant bit to keep in the result.
; right is the index of the least significant bit to keep in the result.
; This function does not sign-extend the result.
(define (unsigned-slice v left [right left])
  (bitwise-bit-field v right (add1 left)))

; Returns a value with the w rightmost bits of v.
; This function does not sign-extend the result.
(define (unsigned v w)
  (unsigned-slice v (sub1 w) 0))

; Returns a sign-extended slice of a value.
; left is the index of the most significant bit to keep in the result.
; right is the index of the least significant bit to keep in the result.
(define (signed-slice v left [right left])
  (define s (unsigned-slice v left right))
  (if (bitwise-bit-set? v left)
    (bitwise-ior s (arithmetic-shift -1 (- left right)))
    s))

; Returns a signed integer value with the given width.
(define (signed v w)
  (signed-slice v (sub1 w) 0))

; Macros to concatenate a sequence of slices.
; Each slice is defined as a list [val left right] or [val left]
; When right is missing, it defaults to left.
(begin-for-syntax
  (define-syntax-class slice-item
    (pattern [val left (~optional right #:defaults ([right #'left]))])))

(define-syntax-parser concat-slices
  [(_ slicer sl ...+ it:slice-item)
   ; Concatenate `sl ...` recursively.
   ; Shift the result left by the width of `it`.
   ; Combine the result with `it` as an unsigned slice.
   #'(bitwise-ior
       (arithmetic-shift (concat-slices slicer sl ...) (add1 (- it.left it.right)))
       (unsigned-slice it.val it.left it.right))]
  [(_ slicer it:slice-item)
   ; Slice the leftmost slice with the given slicing function.
   #'(slicer it.val it.left it.right)])

; Concatenate a sequence of slices.
; Sign-extend the result.
(define-simple-macro (unsigned-concat it ...)
  (concat-slices unsigned-slice it ...))

; Concatenate a sequence of slices.
; Sign-extend the result.
(define-simple-macro (signed-concat it ...)
  (concat-slices signed-slice it ...))

(define (integer->bit-string size v)
  (list->string (for/list ([n (in-range (sub1 size) -1 -1)])
                  (if (bitwise-bit-set? v n) #\1 #\0))))
