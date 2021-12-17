; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require brag/support)

(provide tokenize)

(define (tokenize ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define hydromel-lexer
    (lexer-src-pos
      [(:or "import" "as"
            "interface" "component" "end"
            "type" "port" "in" "out" "flip" "splice"
            "constant" "instance" "signal"
            "or" "and" "not" "xor" "as" ">=" "<=" "==" "/=" ".." "<<" ">>" "=>" "<-"
            "if" "then" "else" "elseif" "case" "of" "for" "loop"
            "register" "when"
            (char-set ".:,()[]{}=+-*/<>_"))
       (token lexeme (string->symbol lexeme))]
      [(:seq alphabetic (:* (:or alphabetic numeric (char-set "_") "::")))
       (token 'ID (string->symbol lexeme))]
      [(:+ numeric)
       (token 'INT (string->number lexeme))]
      [(:seq "b'" (:+ (char-set "01")))
       (token 'INT (string->number (string-replace lexeme "b'" "#b")))]
      [(:seq "x'" (:+ (char-set "0123456789abcdefABCDEF")))
       (token 'INT (string->number (string-replace lexeme "x'" "#x")))]
      [(from/to "\"" "\"")
       (token 'STRING (trim-ends "\"" lexeme "\""))]
      [(from/stop-before "#" "\n")
       (token 'COMMENT lexeme #:skip? #t)]
      [whitespace
       (token 'WS lexeme #:skip? #t)]
      [(eof)
       (void)]))
  (define (next-token) (hydromel-lexer ip))
  next-token)
