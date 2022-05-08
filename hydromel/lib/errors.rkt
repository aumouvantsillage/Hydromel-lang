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

; Read the Hydromel source code that corresponds to a given syntax object.
; This function extracts the source and position information from the syntax
; object and reads the corresponding text in the source file.
(define (syntax-read-from-source stx)
  (with-handlers ([exn:fail:filesystem? (const "(No source)")])
    (with-input-from-file (syntax-source stx)
      (thunk
        (read-string (sub1 (syntax-position stx)))
        (string-trim (read-string (syntax-span stx)))))))

; Extract a subexpression from `stx` at the given position `pos`.
; The actual location of the subexpression depends on the parent syntax object.
; If `pos` is a number, we are checking an expression inside the syntax object.
; If `pos` is #f, we are checking the type field of the syntax object.
(define (subexpr-at-offset stx pos)
  (define n (syntax-parse stx
              #:datum-literals [call-expr data-port parameter constant local-signal]
                                                       ; pos = number   #f
              [(call-expr    name arg ...)   (+ pos 2)]      ; arg[pos] N/A
              [(constant     name expr)      2]              ; expr     N/A
              [(data-port    name mode type) 3]              ; N/A      type
              [(parameter    name type)      (if pos 1 2)]   ; name     type
              [(local-signal name expr)      2]              ; expr     N/A
              [(local-signal name type expr) (if pos 3 2)])) ; expr     type
  (nth (stx->list stx) n))

; Raise a semantic error for a given expression.
; In a typical use of this function, `subexpr` is the problematic expression
; and `expr` is a parent expression provided for context.
; If `subexpr` is false, the error will refer to `expr`.
(define (raise-semantic-error msg expr [subexpr #f])
  (if subexpr
    (let ([subexpr^ (if (syntax? subexpr)
                      subexpr
                      (subexpr-at-offset expr subexpr))])
      (raise-user-error 'ERROR "~a\n  at: ~a\n  in: ~a\n  location: ~a" msg
                        (syntax-read-from-source subexpr^)
                        (syntax-read-from-source expr)
                        (srcloc->string (syntax-srcloc subexpr^))))
    (raise-user-error 'ERROR "~a\n  at: ~a\n  location: ~a" msg
                      (syntax-read-from-source expr)
                      (srcloc->string (syntax-srcloc expr)))))

; Raise a type error for a given expression.
; `expr` is a parent expression (e.g. a call expression) and `pos` is the
; location of a problematic subexpression of `expr` (e.g. an argument).
(define (raise-type-error expr pos actual-type expected-type)
  (define subexpr (subexpr-at-offset expr pos))
  (raise-user-error 'ERROR "Incompatible type\n  expected: ~a\n  found: ~a\n  at: ~a\n  in: ~a\n  location: ~a"
                    expected-type
                    actual-type
                    (syntax-read-from-source subexpr)
                    (syntax-read-from-source expr)
                    (srcloc->string (syntax-srcloc subexpr))))
