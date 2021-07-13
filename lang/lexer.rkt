#lang racket

(require brag/support)

(provide tokenize)

(define (tokenize ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define hydromel-lexer
    (lexer-src-pos
      [(:or "interface" "component" "end"
            "type" "port" "in" "out" "flip" "splice"
            "constant" "instance" "signal"
            "or" "and" "not" ">=" "<=" "==" "/="
            "if" "then" "else"
            "register" "when"
            (char-set ".:,()[]=+-*/<>"))
       (token lexeme (string->symbol lexeme))]
      [(:seq alphabetic (:* (:or alphabetic numeric)))
       (token 'ID (string->symbol lexeme))]
      [(:+ numeric)
       (token 'INT (string->number lexeme))]
      [(from/stop-before "#" "\n")
       (token 'COMMENT lexeme #:skip? #t)]
      [whitespace
       (token 'WS lexeme #:skip? #t)]
      [(eof)
       (void)]))
  (define (next-token) (hydromel-lexer ip))
  next-token)
