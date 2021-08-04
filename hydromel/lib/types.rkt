#lang racket

(provide (all-defined-out))

(struct signed   (width)          #:transparent)
(struct unsigned (width)          #:transparent)
(struct tuple    (elt-types)      #:transparent)
(struct array    (sizes elt-type) #:transparent)
(struct record   (fields)         #:transparent)
