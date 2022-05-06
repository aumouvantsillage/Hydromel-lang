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

; TODO Offset can be wrong when checking a type instead of an expression
(define (offset stx)
  (syntax-parse stx
    #:datum-literals [call-expr data-port parameter constant local-signal]
    [(call-expr    _ ...) 2]
    [(data-port    _ ...) 3]
    [(parameter    _ ...) 1]
    [(constant     _ ...) 2]
    [(local-signal _ _)   2]
    [(local-signal _ _ _) 3]))

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
  (define subexpr (nth (stx->list expr) (+ pos (offset expr))))
  (raise-user-error 'ERROR "Incompatible type\n  expected: ~a\n  found: ~a\n  at: ~a\n  in: ~a\n  location: ~a"
                    expected-type
                    actual-type
                    (syntax-read-from-source subexpr)
                    (syntax-read-from-source expr)
                    (srcloc->string (syntax-srcloc subexpr))))
