; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  racket/syntax-srcloc
  syntax/stx
  syntax/parse
  (only-in data/collection nth))

(provide
  raise-semantic-error
  raise-type-error)

(define (syntax-read-from-source stx)
  (with-handlers ([exn:fail:filesystem? (const "(No source)")])
    (with-input-from-file (syntax-source stx)
      (thunk
        (read-string (sub1 (syntax-position stx)))
        (string-trim (read-string (syntax-span stx)))))))

; pos = #f means that we are checking the type field of stx.
(define (apply-offset pos stx)
  (syntax-parse stx
    #:datum-literals [call-expr data-port parameter constant local-signal]
                                             ; pos = number   #f
    [(call-expr    name arg ...)   (+ pos 2)]      ; arg[pos] N/A
    [(constant     name expr)      2]              ; expr     N/A
    [(data-port    name mode type) 3]              ; N/A      type
    [(parameter    name type)      (if pos 1 2)]   ; name     type
    [(local-signal name expr)      2]              ; expr     N/A
    [(local-signal name type expr) (if pos 3 2)])) ; expr     type

(define (raise-semantic-error msg expr [subexpr #f])
  (if subexpr
    (let ([subexpr^ (if (syntax? subexpr)
                      subexpr
                      (nth (stx->list expr) (+ 2 subexpr)))])
      (raise-user-error 'ERROR "~a\n  at: ~a\n  in: ~a\n  location: ~a" msg
                        (syntax-read-from-source subexpr^)
                        (syntax-read-from-source expr)
                        (srcloc->string (syntax-srcloc subexpr))))
    (raise-user-error 'ERROR "~a\n  at: ~a\n  location: ~a" msg
                      (syntax-read-from-source expr)
                      (srcloc->string (syntax-srcloc expr)))))

(define (raise-type-error expr pos actual-type expected-type)
  (displayln expr)
  (define subexpr (nth (stx->list expr) (apply-offset pos expr)))
  (raise-user-error 'ERROR "Incompatible type\n  expected: ~a\n  found: ~a\n  at: ~a\n  in: ~a\n  location: ~a"
                    expected-type
                    actual-type
                    (syntax-read-from-source subexpr)
                    (syntax-read-from-source expr)
                    (srcloc->string (syntax-srcloc subexpr))))
