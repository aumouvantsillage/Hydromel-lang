; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang s-exp syntax/module-reader
hydromel/lang/expander
#:read                hydromel-read
#:read-syntax         hydromel-read-syntax
#:whole-body-readers? #t

(require "lexer.rkt" "grammar.rkt")

(define (hydromel-read in)
  (syntax->datum (hydromel-read-syntax #f in)))

(define (hydromel-read-syntax src ip)
  (list (parse src (tokenize ip))))
