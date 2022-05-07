; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  threading
  syntax/parse/define
  (for-syntax racket/syntax))

(provide
  min-unsigned-width min-signed-width
  min-signed-value   max-signed-value
  min-unsigned-value max-unsigned-value
  unsigned unsigned-slice set-slice
  unsigned-concat unsigned-concat*
  signed   signed-slice   signed-concat   signed-concat*
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

; Returns the minimum unsigned value with `w` bits.
(define (min-unsigned-value w)
  0)

; Returns the maximum unsigned value with `w` bits.
(define (max-unsigned-value w)
  (sub1 (arithmetic-shift 1 w)))

; Returns the minimum signed value with `w` bits.
(define (min-signed-value w)
  (arithmetic-shift -1 (sub1 w)))

; Returns the maximum signed value with `w` bits.
(define (max-signed-value w)
  (sub1 (arithmetic-shift 1 (sub1 w))))

; Returns a slice of a value.
; `left` is the index of the most significant bit to keep in the result.
; `right` is the index of the least significant bit to keep in the result.
; This function does not sign-extend the result.
(define (unsigned-slice v left [right left])
  (bitwise-bit-field v right (add1 left)))

; Returns a value with the `w` rightmost bits of `v`.
; This function does not sign-extend the result.
(define (unsigned v w)
  (unsigned-slice v (sub1 w) 0))

; Returns a sign-extended slice of a value.
; `left` is the index of the most significant bit to keep in the result.
; `right` is the index of the least significant bit to keep in the result.
(define (signed-slice v left [right left])
  (define s (unsigned-slice v left right))
  (if (bitwise-bit-set? v left)
    (bitwise-ior s (arithmetic-shift -1 (- left right)))
    s))

; Returns a value with the `w` rightmost bits of `v`.
; This function sign-extends the result.
(define (signed v w)
  (signed-slice v (sub1 w) 0))

; Modifies a slive of `v`.
; `left` is the index of the most significant bit to change.
; `right` is the index of the least significant bit to change.
; `subst` is the value to copy into the specified slice of `v`.
; If the slice width is `w`, only the `w` least significant bits of `subst` are copied.
(define (set-slice v left right subst)
  (define mask (bitwise-ior (arithmetic-shift -1 (add1 left))
                            (sub1 (arithmetic-shift 1 right))))
  (bitwise-ior (bitwise-and v mask)
               (bitwise-and (arithmetic-shift subst right) (bitwise-not mask))))

; Concatenate a sequence of slices.
; Do not sign-extend the result.
(define (unsigned-concat* . items)
  (for/fold ([res 0])
            ([it (in-list items)])
    (match-define (list v l r) it)
    (~> res
        (arithmetic-shift (add1 (- l r)))
        (bitwise-ior (unsigned-slice v l r)))))

; Helper macro to concatenate a sequence of slices.
; Do not sign-extend the result.
(define-syntax-parse-rule (unsigned-concat [v l r] ...)
  (unsigned-concat* [list v l r] ...))

; Concatenate a sequence of slices.
; Sign-extend the result.
(define (signed-concat* . items)
  (match-define (list v0 l0 r0) (first items))
  (for/fold ([res (signed-slice v0 l0 r0)])
            ([it (in-list (rest items))])
    (match-define (list v l r) it)
    (~> res
        (arithmetic-shift (add1 (- l r)))
        (bitwise-ior (unsigned-slice v l r)))))

; Helper macro to concatenate a sequence of slices into a sign-extended value.
(define-syntax-parse-rule (signed-concat [v l r] ...)
  (signed-concat* [list v l r] ...))

; Convert an integer into a binary string of the given width.
(define (integer->bit-string size v)
  (list->string (for/list ([n (in-range (sub1 size) -1 -1)])
                  (if (bitwise-bit-set? v n) #\1 #\0))))
