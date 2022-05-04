; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require racket/syntax-srcloc)

(provide raise-semantic-error)

(define (syntax-read-from-source stx)
  (with-input-from-file (syntax-source stx)
    (thunk
      (read-string (sub1 (syntax-position stx)))
      (string-trim (read-string (syntax-span stx))))))

(define (raise-semantic-error msg expr [subexpr #f])
  (if subexpr
    (raise-user-error 'ERROR "~a\n  at: ~a\n  in: ~a\n  location: ~a" msg
                      (syntax-read-from-source subexpr)
                      (syntax-read-from-source expr)
                      (srcloc->string (syntax-srcloc subexpr)))
    (raise-user-error 'ERROR "~a\n  at: ~a\n  location: ~a" msg
                      (syntax-read-from-source expr)
                      (srcloc->string (syntax-srcloc expr)))))
