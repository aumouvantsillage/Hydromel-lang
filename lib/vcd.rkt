; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  syntax/parse/define
  "logic.rkt"
  "logic-vector.rkt"
  "signal.rkt"
  "std.rkt")

(provide vcd)

(struct waveform (short-name width values))

(define (vcd table duration ts [out (current-output-port)])
  (define wavs (for/hash ([(name slt) (in-dict table)]
                          [index      (in-naturals)])
                 (define samples (signal-take (slot-signal slt) duration))
                 (values name (waveform (format "s~a" index)
                                        (apply max (map logic-vector-width samples))
                                        (map logic-vector-value samples)))))

  ; VCD header.
  (fprintf out "$timescale ~a $end\n" ts)
  (for ([(name wav) (in-dict wavs)])
    (fprintf out "$var wire ~a ~a ~a $end\n" (waveform-width wav) (waveform-short-name wav) name))
  (fprintf out "$enddefinitions $end\n")

  ; Value changes.
  (define short-names (map waveform-short-name (dict-values wavs)))
  (define widths      (map waveform-width      (dict-values wavs)))
  (for/fold ([it   (map waveform-values (dict-values wavs))]
             [prev (map void (dict-keys wavs))] ; This will force a value change at t=0
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
        (fprintf out fmt (integer->bit-string w v) n)))

    ; Continue with the rest of the signals.
    (values
      (map rest it)
      current))

  ; Last timestamp
  (fprintf out "#~a\n" duration))
