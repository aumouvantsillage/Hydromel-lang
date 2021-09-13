; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang brag

; We follow this pattern when we want to prevent the parser
; from splicing a list of items in the AST node for a rule:
;
; a: a-item-list
; /a-item-list: a-item*
;
; This will create an AST node of the form: (a (a-item ...))

begin-hydromel: (import | interface | component | constant | type | function)*

import: /"import" STRING (/"as" ID)?

interface: /"interface" ID parameter-list? interface-item* /"end"

component: /"component" ID parameter-list? component-item* /"end"

@parameter-list: /"(" (parameter /",")* parameter? /")"

@interface-item:
  data-port |
  composite-port |
  constant |
  type

@component-item:
  interface-item |
  statement

@statement:
  local-signal |
  assignment |
  instance |
  if-statement |
  for-statement

parameter: ID /":" type-expression

constant: /"constant" ID /"=" expression

type: /"type" ID parameter-list? (/"=" type-expression)?

function: /"function" ID parameter-list (/"=" expression)?

local-signal: /"signal" ID (/":" type-expression)? /"=" expression

data-port: /"port" ID /":" ("in" | "out") type-expression

composite-port: /"port" ID multiplicity? /":" composite-mode* ID argument-list?

@composite-mode: "flip" | "splice"

multiplicity: /"<" expression /">"

; TODO named arguments
@argument-list: /"(" (expression /",")* expression? /")"

assignment:
  simple-expr /"=" expression

instance:
  /"instance" ID multiplicity? /"=" ID argument-list?

if-statement:
  (ID /":")?
  /"if" expression /"then" statement-block
  (/"elseif" expression /"then" statement-block)*
  (/"else" statement-block)?
  /"end"

for-statement:
  (ID /":")?
  /"for" ID /"in" expression /"loop"
  statement-block
  /"end"

statement-block:
  statement*

; Expressions ------------------------------------------------------------------

; TODO other expressions
@expression: maybe-if-expr

@maybe-if-expr:     if-expr     | maybe-or-expr
@maybe-or-expr:     or-expr     | maybe-and-expr
@maybe-and-expr:    and-expr    | maybe-rel-expr
@maybe-rel-expr:    rel-expr    | maybe-range-expr
@maybe-range-expr:  range-expr  | maybe-add-expr
@maybe-add-expr:    add-expr    | maybe-mult-expr
@maybe-mult-expr:   mult-expr   | maybe-prefix-expr
@maybe-prefix-expr: prefix-expr | simple-expr

if-expr:
  /"if" expression /"then" expression else-clause

@else-clause:
  /"else" expression |
  /"elseif" expression /"then" expression else-clause

or-expr:     maybe-or-expr  ("or" | "xor") maybe-and-expr
and-expr:    maybe-and-expr "and" maybe-rel-expr
rel-expr:    maybe-add-expr ("<" | ">" | "<=" | ">=" | "==" | "/=" | "in") maybe-range-expr
range-expr:  maybe-add-expr ".." maybe-add-expr
add-expr:    maybe-add-expr ("+" | "-") maybe-mult-expr
mult-expr:   maybe-mult-expr ("*" | "/") maybe-prefix-expr
prefix-expr: ("-" | "not") simple-expr

; TODO Add comprehensions
@simple-expr:
  literal-expr |
  name-expr |
  field-expr |
  indexed-port-expr |
  indexed-array-expr |
  slice-expr |
  concat-expr |
  register-expr |
  call-expr |
  /"(" expression /")"

literal-expr: INT

name-expr: ID

field-expr:
  simple-expr /"." ID

indexed-port-expr:
  simple-expr /"<" expression /">"

indexed-array-expr:
  simple-expr /"[" expression /"]"

slice-expr:
  simple-expr /"{" expression /"}"

concat-expr:
  /"{" expression (/"," expression)* /","? /"}"

register-expr:
  /"register" /"(" expression when-clause? /"," expression when-clause? /")"

when-clause:
  /"when" expression

call-expr:
  ID /"(" (expression /",")* expression? /")"

; Type expressions -------------------------------------------------------------

; Array and tuple types are included in call-expr
@type-expression:
  "type" |
  name-expr |
  call-expr |
  record-type

record-type:
  /"record" /"(" record-field (/"," record-field)* ","? /")"

; TODO spliced fields?
/record-field:
  ID /":" type-expression
