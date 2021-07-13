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
