; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  threading
  (only-in "numeric.rkt" integer->bit-string)
  (only-in "types.rkt" minimize abstract-integer-type array-type)
  (only-in data/collection nth)
  "signal.rkt"
  "slot.rkt"
  "instance.rkt")

(provide instance-dump-vcd)

; A VCD waveform definition contains the "short" name of a signal,
; its bit width, and a list of values.
(struct waveform (short-name width values))

; Dump the list of variable definitions of an instance.
; This function will traverse the hierarchy of instances and create
; a VCD scope statement for each inner instance.
; It returns a list of waveforms.
(define (instance-dump-vcd-vars inst duration scope path out)
  (match inst
    [(slot _ sig _ _) #:when sig
     (define samples (if (signal? sig)
                       (signal-take sig duration)
                       (make-list duration sig)))
     (match (minimize (slot-type inst))
       [(abstract-integer-type w)
        (fprintf out "$var wire ~a ~a ~a $end\n" w path scope)
        (waveform path w samples)]
       [(array-type n te)
        (instance-dump-vcd-array samples n te scope path out)]
       ; TODO support records
       [_ (error "Unsupported data type at" path)])]

    [(hash-table _ ...)
     ; Open a new scope in the VCD file.
     (fprintf out "$scope module ~a $end\n" scope)
     ; Get the list of spliced port names.
     ; Include '* as a key to avoid when processing the hash-table.
     (define sp (cons '* (dict-ref inst '* (thunk empty))))
     ; For each field that is not a spliced port...
     (define res (for/list ([(k v) (in-dict inst)]
                            [n (in-naturals)]
                            #:when (not (member k sp)))
                   (instance-dump-vcd-vars v duration k (format "~a_~a" path n) out)))
     ; Close the scope in the VCD file.
     (fprintf out "$upscope $end\n")
     (flatten res)]

    [(vector elt ...)
     ; Open a new scope in the VCD file.
     (fprintf out "$scope module ~a $end\n" scope)
     ; For each index and each element of the vector...
     (define res (for/list ([(v n) (in-indexed elt)])
                   (instance-dump-vcd-vars v duration n (format "~a_~a" path n) out)))
     ; Close the scope in the VCD file.
     (fprintf out "$upscope $end\n")
     (flatten res)]

    [_ (error "Unsupported data type at" path)]))

; Dump VCD variables for a signal of array type.
; Due to limitations of the VCD format, we cannot dump an array as a single value.
; This function will create a new VCD scope for an array and will dump one
; variable for each element.
; It returns a list of waveforms.
(define (instance-dump-vcd-array samples len elt-type scope path out)
  ; Open a new scope in the VCD file.
  (fprintf out "$scope module ~a $end\n" scope)
  (define res (for/list ([i (in-range len)])
                (define lst (for/list ([s (in-list samples)])
                              (nth s i)))
                (define path^ (format "~a_~a" path i))
                (match elt-type
                  [(abstract-integer-type w)
                   (fprintf out "$var wire ~a ~a ~a $end\n" w path^ i)
                   (waveform path^ w lst)]
                  [(array-type n te) (instance-dump-vcd-array lst n te i path^ out)]
                  ; TODO support records
                  [_ (error "Unsupported data type at" path)])))
  ; Close the scope in the VCD file.
  (fprintf out "$upscope $end\n")
  (flatten res))

; Dump the slots of an instance to an output port using the VCD format.
(define (instance-dump-vcd inst duration ts [out (current-output-port)])
  ; VCD header.
  (fprintf out "$timescale ~a $end\n" ts)
  (define wavs (instance-dump-vcd-vars inst duration "top" "s" out))
  (fprintf out "$enddefinitions $end\n")

  ; Value changes.
  (define short-names (map waveform-short-name wavs))
  (define widths      (map waveform-width      wavs))
  (for/fold ([it   (map waveform-values wavs)]
             [prev (map void wavs)] ; This will force a value change at t=0
             #:result (void))
            ([t (in-range duration)])
    (define current (map first it))

    ; If at least one signal changed.
    (unless (equal? current prev)
      ; Output a timestamp
      (fprintf out "#~a\n" t)

      ; Output value changes.
      (for ([n (in-list short-names)]
            [w (in-list widths)]
            [v (in-list current)]
            [p (in-list prev)]
            #:when (not (equal? v p)))
        (define fmt (if (= w 1)
                      "~a~a\n"
                      "b~a ~a\n"))
        (fprintf out fmt (integer->bit-string v w) n)))

    ; Continue with the rest of the signals.
    (values
      (map rest it)
      current))

  ; Last timestamp
  (fprintf out "#~a\n" duration))
