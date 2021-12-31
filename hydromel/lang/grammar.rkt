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

begin-hydromel: (import | interface | component | constant | typedef | function)*

import: /"import" STRING (/"as" ID)?

interface: /"interface" ID parameter-list? interface-item* /"end"

component: /"component" ID parameter-list? component-item* /"end"

@parameter-list: /"(" (parameter /",")* parameter? /")"

@interface-item:
  data-port |
  composite-port |
  constant |
  typedef

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

typedef: /"type" ID parameter-list? (/"=" type-expression)?

function: /"function" ID parameter-list (/"=" expression)?

local-signal: /"signal" ID (/":" type-expression)? /"=" expression

data-port: /"port" ID /":" ("in" | "out") type-expression

composite-port: /"port" ID multiplicity /":" composite-mode* ID argument-list?

@composite-mode: "flip" | "splice"

/multiplicity: (/"<" (expression /",")* expression /","? /">")?

; TODO named arguments
@argument-list: /"(" (expression /",")* expression? /")"

assignment:
  simple-expr /"=" expression

instance:
  /"instance" ID multiplicity /"=" ID argument-list?

if-statement:
  (ID /":")?
  /"if" expression /"then" statement-block
  (/"elseif" expression /"then" statement-block)*
  (/"else" statement-block)?
  /"end"

for-statement:
  (ID /":")?
  /"for" iterator /"loop"
  statement-block
  /"end"

@iterator:
  ID /"in" expression

@iterator-list:
  iterator (/"," iterator)* /","?

statement-block:
  statement*

; Expressions ------------------------------------------------------------------

@expression: maybe-assign-expr

@expression-list:
  expression (/"," expression)* /","?

@maybe-assign-expr: assign-expr | maybe-cond-expr
@maybe-cond-expr:   cond-expr   | maybe-or-expr
@maybe-or-expr:     or-expr     | maybe-and-expr
@maybe-and-expr:    and-expr    | maybe-rel-expr
@maybe-rel-expr:    rel-expr    | maybe-range-expr
@maybe-range-expr:  range-expr  | maybe-add-expr
@maybe-add-expr:    add-expr    | maybe-mult-expr
@maybe-mult-expr:   mult-expr   | maybe-shift-expr
@maybe-shift-expr:  shift-expr  | maybe-prefix-expr
@maybe-prefix-expr: prefix-expr | maybe-cast-expr
@maybe-cast-expr:   cast-expr   | simple-expr

; TODO support slice-expr and field-expr
assign-expr:
  indexed-array-expr /"<-" maybe-cond-expr

@cond-expr:
  if-expr |
  case-expr

if-expr:
  /"if" expression /"then" expression else-clause

@else-clause:
  /"else" /"if" expression /"then" expression else-clause |
  /"elseif"     expression /"then" expression else-clause |
  /"else"                          expression

case-expr:
  /"case" expression /"of"
    (case-clause+ |
     case-clause* case-default-clause)

@case-clause:
  choices /"=>" expression

@case-default-clause:
  /"_" /"=>" expression

choices:
  expression-list

or-expr:     maybe-or-expr  ("or" | "xor") maybe-and-expr
and-expr:    maybe-and-expr "and" maybe-rel-expr
rel-expr:    maybe-add-expr ("<" | ">" | "<=" | ">=" | "==" | "/=" | "in") maybe-range-expr
range-expr:  maybe-add-expr ".." maybe-add-expr
add-expr:    maybe-add-expr ("+" | "-") maybe-mult-expr
mult-expr:   maybe-mult-expr ("*" | "/") maybe-shift-expr
shift-expr:  maybe-shift-expr ("<<" | ">>") maybe-prefix-expr
prefix-expr: ("-" | "not") maybe-cast-expr
cast-expr:   simple-expr /"as" type-expression

@simple-expr:
  literal-expr |
  name-expr |
  field-expr |
  indexed-port-expr |
  indexed-array-expr |
  slice-expr |
  concat-expr |
  concat-for-expr |
  array-expr |
  array-for-expr |
  record-expr |
  register-expr |
  call-expr |
  /"(" expression /")"

literal-expr: INT | SYMBOL

name-expr: ID | "type"

field-expr:
  simple-expr /"." ID

indexed-port-expr:
  simple-expr /"<" expression /">"

indexed-array-expr:
  simple-expr /"[" expression /"]"

slice-expr:
  simple-expr /"{" expression /"}"

concat-expr:
  /"{" expression-list /"}"

concat-for-expr:
  /"{" expression /"for" iterator-list /"}"

array-expr:
  /"[" expression-list /"]"

array-for-expr:
  /"[" expression /"for" iterator-list /"]"

record-expr:
  /"(" @field-assoc (/"," @field-assoc )* /","? /")"

field-assoc:
  ID /"=>" expression

register-expr:
  /"register" /"(" expression when-clause? /"," expression when-clause? /")"

when-clause:
  /"when" expression

call-expr:
  ID /"(" (expression /",")* expression? /")"

; Type expressions -------------------------------------------------------------

; Array and tuple types are included in call-expr
@type-expression:
  name-expr |
  call-expr |
  record-type

record-type:
  /"record" /"(" record-field (/"," record-field)* /","? /")"

; TODO spliced fields?
@record-field:
  ID /":" type-expression
