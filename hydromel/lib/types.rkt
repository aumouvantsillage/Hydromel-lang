#lang racket

(provide (all-defined-out))

(struct integer  (width)          #:transparent)
(struct signed   integer ()       #:transparent)
(struct unsigned integer ()       #:transparent)
(struct tuple    (elt-types)      #:transparent)
(struct array    (sizes elt-type) #:transparent)
(struct record   (fields)         #:transparent)
