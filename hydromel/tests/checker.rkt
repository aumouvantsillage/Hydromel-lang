; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  data/pvector
  "../lib/checker.rkt"
  "../lib/expander.rkt"
  "../lib/signal.rkt"
  "../lib/std.rkt"
  "../lib/types.rkt"
  "../lib/slot.rkt"
  "../lib/instance.rkt")

(define (check-sig-equal? t e n)
  (check-equal? (signal-take t n) (signal-take e n)))

(begin-hydromel
  (component C0
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (name-expr x))))

(define c0-inst (C0))
(instance-set! c0-inst 'x (signal 10))

(test-case "Can label a simple signal expressions"
  (check-sig-equal? (instance-ref c0-inst 'y) (instance-ref c0-inst 'x) 5))

(begin-hydromel
  (interface I0
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32))))

  (component C1
    (composite-port i () I0)
    (assignment (field-expr (name-expr i) y) (field-expr (name-expr i) x))))

(define c1-inst (C1))
(instance-set! c1-inst '(i x) (signal 10))

(test-case "Can resolve ports in field expressions"
  (check-sig-equal? (instance-ref c1-inst '(i y)) (instance-ref c1-inst '(i x)) 5))

(begin-hydromel
  (component C2
    (composite-port i ((literal-expr 2)) I0)
    (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 0)) y)
                (field-expr (indexed-port-expr (name-expr i) (literal-expr 0)) x))
    (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 1)) y)
                (field-expr (indexed-port-expr (name-expr i) (literal-expr 1)) x))))

(define c2-inst (C2))
(instance-set! c2-inst '(i 0 x) (signal 10))
(instance-set! c2-inst '(i 1 x) (signal 20))

(test-case "Can resolve ports in indexed expressions"
  (check-sig-equal? (instance-ref c2-inst '(i 0 y)) (instance-ref c2-inst '(i 0 x)) 5)
  (check-sig-equal? (instance-ref c2-inst '(i 1 y)) (instance-ref c2-inst '(i 1 x)) 5))

(begin-hydromel
  (interface I1
    (composite-port i ((literal-expr 2)) I0))

  (component C3
    (composite-port j ((literal-expr 2)) I1)
    (assignment (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 0)) i) (literal-expr 0)) y)
                (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 0)) i) (literal-expr 0)) x))
    (assignment (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 0)) i) (literal-expr 1)) y)
                (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 0)) i) (literal-expr 1)) x))
    (assignment (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 1)) i) (literal-expr 0)) y)
                (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 1)) i) (literal-expr 0)) x))
    (assignment (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 1)) i) (literal-expr 1)) y)
                (field-expr (indexed-port-expr (field-expr (indexed-port-expr (name-expr j) (literal-expr 1)) i) (literal-expr 1)) x))))

(define c3-inst (C3))
(instance-set! c3-inst '(j 0 i 0 x) (signal 10))
(instance-set! c3-inst '(j 0 i 1 x) (signal 20))
(instance-set! c3-inst '(j 1 i 0 x) (signal 30))
(instance-set! c3-inst '(j 1 i 1 x) (signal 40))

(test-case "Can resolve ports in a hierarchy of expressions"
  (check-sig-equal? (instance-ref c3-inst '(j 0 i 0 y)) (instance-ref c3-inst '(j 0 i 0 x)) 5)
  (check-sig-equal? (instance-ref c3-inst '(j 0 i 1 y)) (instance-ref c3-inst '(j 0 i 1 x)) 5)
  (check-sig-equal? (instance-ref c3-inst '(j 1 i 0 y)) (instance-ref c3-inst '(j 1 i 0 x)) 5)
  (check-sig-equal? (instance-ref c3-inst '(j 1 i 1 y)) (instance-ref c3-inst '(j 1 i 1 x)) 5))

(begin-hydromel
  (component C4
    (data-port x out (call-expr signed (literal-expr 32)))
    (assignment (name-expr x) (literal-expr 10))))

(define c4-inst (C4))

(test-case "Can assign a literal to a signal"
  (check-sig-equal? (instance-ref c4-inst 'x) (signal 10) 5))

(begin-hydromel
  (component C5
    (data-port x out (call-expr signed (literal-expr 32)))
    (constant k (literal-expr 10))
    (assignment (name-expr x) (name-expr k))))

(define c5-inst (C5))

(test-case "Can assign a constant to a signal"
  (check-sig-equal? (instance-ref c5-inst 'x) (signal 10) 5))

(begin-hydromel
  (component C6
    (data-port x out (call-expr signed (literal-expr 32)))
    (constant k (literal-expr 10))
    (assignment (name-expr x) (add-expr (name-expr k) + (literal-expr 1)))))

(define c6-inst (C6))

(test-case "Can assign a static expression to a signal"
  (check-sig-equal? (instance-ref c6-inst 'x) (signal 11) 5))

(begin-hydromel
  (component C7
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z)
      (cast-expr
        (call-expr signed (literal-expr 32))
        (add-expr (name-expr x) + (name-expr y))))))

(define c7-inst (C7))
(instance-set! c7-inst 'x (signal 10 20 30))
(instance-set! c7-inst 'y (signal 40 50 60))

(test-case "Can lift an operation"
  (check-sig-equal? (instance-ref c7-inst 'z) (signal 50 70 90) 5))

(begin-hydromel
  (component C8
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u in (call-expr signed (literal-expr 32)))
    (data-port v out (call-expr signed (literal-expr 32)))
    (assignment (name-expr v)
      (cast-expr
        (call-expr signed (literal-expr 32))
        (add-expr (mult-expr (name-expr x) * (name-expr y))
                + (mult-expr (name-expr z) * (name-expr u)))))))

(define c8-inst (C8))
(instance-set! c8-inst 'x (signal 10 20 30 40 50))
(instance-set! c8-inst 'y (signal 2))
(instance-set! c8-inst 'z (signal 1 2 3 4 5))
(instance-set! c8-inst 'u (signal 3))

(test-case "Can lift nested calls"
  (check-sig-equal? (instance-ref c8-inst 'v) (signal 23 46 69 92 115) 5))

(begin-hydromel
  (component C9
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u in (call-expr signed (literal-expr 32)))
    (data-port v out (call-expr signed (literal-expr 32)))
    (local-signal xy (mult-expr (name-expr x) * (name-expr y)))
    (local-signal zu (mult-expr (name-expr z) * (name-expr u)))
    (assignment (name-expr v)
      (cast-expr
        (call-expr signed (literal-expr 32))
        (add-expr (name-expr xy) + (name-expr zu))))))

(define c9-inst (C9))
(instance-set! c9-inst 'x (signal 10 20 30 40 50))
(instance-set! c9-inst 'y (signal 2))
(instance-set! c9-inst 'z (signal 1 2 3 4 5))
(instance-set! c9-inst 'u (signal 3))

(test-case "Can use local signals"
  (check-sig-equal? (instance-ref c9-inst 'v) (signal 23 46 69 92 115) 5))

(begin-hydromel
  (interface I2
    (data-port x in (call-expr signed (literal-expr 32))))

  (component C10
    (composite-port i ((literal-expr 3)) I2)
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z)
                (field-expr (indexed-port-expr (name-expr i) (name-expr y)) x))))

(define c10-inst (C10))
(instance-set! c10-inst '(i 0 x) (signal 10))
(instance-set! c10-inst '(i 1 x) (signal 20))
(instance-set! c10-inst '(i 2 x) (signal 30))
(instance-set! c10-inst 'y     (signal 0 1 2 1 0 2))

(test-case "Can access simple ports in a vector composite port with dynamic indices"
  (check-sig-equal? (instance-ref c10-inst 'z) (signal 10 20 30 20 10 30) 5))

(begin-hydromel
  (component C11
    (parameter N (call-expr unsigned (literal-expr 32)))
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y)
      (cast-expr
        (call-expr signed (literal-expr 32))
        (mult-expr (name-expr x) * (name-expr N)))))

  (component C12
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c () C11 (literal-expr 10))
    (assignment (field-expr (name-expr c) x) (name-expr x))
    (assignment (name-expr y) (field-expr (name-expr c) y))))

(define c12-inst (C12))
(instance-set! c12-inst 'x (signal 10 20 30 40 50))

(test-case "Can instantiate a component"
  (check-sig-equal? (instance-ref c12-inst 'y) (signal 100 200 300 400 500) 5))

(begin-hydromel
  (component C13
    (data-port x0 in (call-expr signed (literal-expr 32)))
    (data-port x1 in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c ((literal-expr 2)) C11 (literal-expr 10))
    (assignment (field-expr (indexed-port-expr (name-expr c) (literal-expr 0)) x) (name-expr x0))
    (assignment (field-expr (indexed-port-expr (name-expr c) (literal-expr 1)) x) (name-expr x1))
    (assignment (name-expr y)
      (cast-expr
        (call-expr signed (literal-expr 32))
        (add-expr (field-expr (indexed-port-expr (name-expr c) (literal-expr 0)) y)
                + (field-expr (indexed-port-expr (name-expr c) (literal-expr 1)) y))))))

(define c13-inst (C13))
(instance-set! c13-inst 'x0 (signal 10 20 30 40 50))
(instance-set! c13-inst 'x1 (signal 1  2  3  4  5))

(test-case "Can instantiate a multiple component"
  (check-sig-equal? (instance-ref c13-inst 'y) (signal 110 220 330 440 550) 5))

(begin-hydromel
  (component C14
    (composite-port i () splice I0)
    (assignment (name-expr y) (name-expr x))))

(define c14-inst (C14))
(instance-set! c14-inst 'x (signal 10))

(test-case "Can resolve ports in a spliced interface"
  (check-sig-equal? (instance-ref c14-inst 'y) (instance-ref c14-inst 'x) 5))

(begin-hydromel
  (component C15
    (composite-port i () splice flip I0)
    (assignment (name-expr x) (name-expr y))))

(define c15-inst (C15))
(instance-set! c15-inst 'y (signal 10))

(test-case "Can resolve ports in a spliced flipped interface"
  (check-sig-equal? (instance-ref c15-inst 'x) (instance-ref c15-inst 'y) 5))

(begin-hydromel
  (component C16
    (composite-port j () splice I1)
    (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 0)) y)
                (field-expr (indexed-port-expr (name-expr i) (literal-expr 0)) x))
    (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 1)) y)
                (field-expr (indexed-port-expr (name-expr i) (literal-expr 1)) x))))

(define c16-inst (C16))
(instance-set! c16-inst '(i 0 x) (signal 10))
(instance-set! c16-inst '(i 1 x) (signal 20))

(test-case "Can resolve ports in a hierarchy from a spliced interface"
  (check-sig-equal? (instance-ref c16-inst '(i 0 y)) (instance-ref c16-inst '(i 0 x)) 5)
  (check-sig-equal? (instance-ref c16-inst '(i 1 y)) (instance-ref c16-inst '(i 1 x)) 5))

(begin-hydromel
  (interface I3
    (composite-port i () splice I0))

  (component C17
    (composite-port j () splice I3)
    (assignment (name-expr y) (name-expr x))))

(define c17-inst (C17))
(instance-set! c17-inst '(j x) (signal 10))

(test-case "Can resolve ports in an interface with a spliced composite port"
  (check-sig-equal? (instance-ref c17-inst '(j y)) (instance-ref c17-inst '(j x)) 5))

(test-case "Can resolve ports in a doubly spliced composite port"
  (check-sig-equal? (instance-ref c17-inst 'y) (instance-ref c17-inst 'x) 5))

(begin-hydromel
  (component C18
    (composite-port j () splice flip I3)
    (assignment (name-expr x) (name-expr y))))

(define c18-inst (C18))
(instance-set! c18-inst 'y (signal 10))

(test-case "Can resolve ports in a doubly spliced flipped-last composite port"
  (check-sig-equal? (instance-ref c18-inst 'x) (instance-ref c18-inst 'y) 5))

(begin-hydromel
  (interface I4
    (composite-port i () splice flip I0))

  (component C19
    (composite-port j () splice I4)
    (assignment (name-expr x) (name-expr y))))

(define c19-inst (C19))
(instance-set! c19-inst 'y (signal 10))

(test-case "Can resolve ports in a doubly spliced flipped-first composite port"
  (check-sig-equal? (instance-ref c19-inst 'x) (instance-ref c19-inst 'y) 5))

(begin-hydromel
  (component C20
    (composite-port j () splice flip I4)
    (assignment (name-expr y) (name-expr x))))

(define c20-inst (C20))
(instance-set! c20-inst 'x (signal 10))

(test-case "Can resolve ports in a doubly spliced doubly-flipped composite port"
  (check-sig-equal? (instance-ref c20-inst 'y) (instance-ref c20-inst 'x) 5))

(begin-hydromel
  (component C21
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (if-expr (rel-expr (name-expr x) > (name-expr y))
                                (name-expr x)
                                (name-expr y)))))

(define c21-inst (C21))
(instance-set! c21-inst 'x (signal 10 20  30  40 50))
(instance-set! c21-inst 'y (signal 1  200 300 4  5))

(test-case "Can compute a conditional signal"
  (check-sig-equal? (instance-ref c21-inst 'z) (signal 10 200 300 40 50) 5))

(begin-hydromel
  (component C22
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (register-expr (literal-expr 0) (name-expr x)))))

(define c22-inst (C22))
(instance-set! c22-inst 'x (signal 10 20  30 40 50))

(test-case "Can register a signal"
  (check-sig-equal? (instance-ref c22-inst 'y) (signal 0  10 20 30 40 50) 6))

(begin-hydromel
  (component C23
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (register-expr (literal-expr 0) (when-clause (name-expr x))
                                             (name-expr y)))))

(define c23-inst (C23))
(instance-set! c23-inst 'x (signal 0  0  0  1  0))
(instance-set! c23-inst 'y (signal 10 20 30 40 50))

(test-case "Can register a signal with reset"
  (check-sig-equal? (instance-ref c23-inst 'z) (signal 0  10 20 30 0  50) 6))

(begin-hydromel
  (component C24
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z out (call-expr signed (literal-expr 32)))
    (assignment (name-expr z) (register-expr (literal-expr 0)
                                             (name-expr y) (when-clause (name-expr x))))))

(define c24-inst (C24))
(instance-set! c24-inst 'x (signal 0  1  0  1  0))
(instance-set! c24-inst 'y (signal 10 20 30 40 50))

(test-case "Can register a signal with enable"
  (check-sig-equal? (instance-ref c24-inst 'z) (signal 0  0  20 20 40) 6))

(begin-hydromel
  (component C25
    (data-port x in (call-expr signed (literal-expr 32)))
    (data-port y in (call-expr signed (literal-expr 32)))
    (data-port z in (call-expr signed (literal-expr 32)))
    (data-port u out (call-expr signed (literal-expr 32)))
    (assignment (name-expr u) (register-expr (literal-expr 0) (when-clause (name-expr x))
                                             (name-expr z) (when-clause (name-expr y))))))

(define c25-inst (C25))
(instance-set! c25-inst 'x (signal 0  0   1  0  0))
(instance-set! c25-inst 'y (signal 0  1   0  1  0))
(instance-set! c25-inst 'z (signal 10 20  30 40 50))

(test-case "Can register a signal with reset and enable"
  (check-sig-equal? (instance-ref c25-inst 'u) (signal 0  0  20 0  40) 6))

(begin-hydromel
  (component C26
    (constant N (literal-expr 56))
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (name-expr N))))

(define c26-inst (C26))

(test-case "Can read a local constant"
  (check-sig-equal? (instance-ref c26-inst 'y) (signal 56) 1))

(test-case "Can read a constant as a channel field"
  (check-equal? (instance-ref c26-inst 'N) 56))

(begin-hydromel
  (interface I5
    (constant N (literal-expr 56))
    (data-port y out (call-expr signed (literal-expr 32))))

  (component C27
    (composite-port p () I5)
    (assignment (field-expr (name-expr p) y) (field-expr (name-expr p) N))))

(define c27-inst (C27))

(test-case "Can read a constant from a port"
  (check-sig-equal? (instance-ref c27-inst '(p y)) (signal 56) 1))

(begin-hydromel
  (component C28
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c () C26)
    (assignment (name-expr y) (field-expr (name-expr c) N))))

(define c28-inst (C28))

(test-case "Can read a constant from an instance"
  (check-sig-equal? (instance-ref c28-inst 'y) (signal 56) 1))

(begin-hydromel
  (component C29
    (data-port y out (call-expr signed (literal-expr 32)))
    (instance c () C27)
    (assignment (name-expr y) (field-expr (field-expr (name-expr c) p) N))))

(define c29-inst (C29))

(test-case "Can read a constant from an instance port"
  (check-sig-equal? (instance-ref c29-inst 'y) (signal 56) 1))

(begin-hydromel
  (constant K0 (literal-expr 44))

  (component C30
    (data-port y out (call-expr signed (literal-expr 32)))
    (assignment (name-expr y) (name-expr K0))))

(define c30-inst (C30))

(test-case "Can read a global constant"
  (check-sig-equal? (instance-ref c30-inst 'y) (signal 44) 1))

(begin-hydromel
  (component C31
    (constant N (literal-expr 240))
    (constant M (literal-expr -16))
    (data-port y out (call-expr unsigned (literal-expr 1)))
    (data-port z out (call-expr unsigned (literal-expr 1)))
    (data-port t out (call-expr unsigned (literal-expr 4)))
    (data-port u out (call-expr signed   (literal-expr 4)))
    (assignment (name-expr y) (slice-expr (name-expr N) (literal-expr 3)))
    (assignment (name-expr z) (slice-expr (name-expr N) (literal-expr 4)))
    (assignment (name-expr t) (slice-expr (name-expr N) (range-expr (literal-expr 5) .. (literal-expr 2))))
    (assignment (name-expr u) (slice-expr (name-expr M) (range-expr (literal-expr 5) .. (literal-expr 2))))))

(define c31-inst (C31))

(test-case "Can read a bit in an integer value"
  (check-sig-equal? (instance-ref c31-inst 'y) (signal 0) 1)
  (check-sig-equal? (instance-ref c31-inst 'z) (signal 1) 1))

(test-case "Can read an unsigned slice in an integer value"
  (check-sig-equal? (instance-ref c31-inst 't) (signal 12) 1))

(test-case "Can read a signed slice in an integer value"
  (check-sig-equal? (instance-ref c31-inst 'u) (signal -4) 2))

(begin-hydromel
  (component C32
    (data-port x in  (call-expr signed (literal-expr 4)))
    (data-port y in  (call-expr signed (literal-expr 4)))
    (data-port z out (call-expr signed (literal-expr 8)))
    (assignment (name-expr z) (concat-expr (name-expr x) (name-expr y)))))

(define c32-inst (C32))
(instance-set! c32-inst 'x (signal 0 5 -2))
(instance-set! c32-inst 'y (signal 0 3 -4))

(test-case "Can concatenate two integers"
  (check-sig-equal? (instance-ref c32-inst 'z) (signal 0 83 -20) 3))

(begin-hydromel
  (component C33
    (data-port x in  (call-expr unsigned (literal-expr 8)))
    (data-port y out (call-expr unsigned (literal-expr 8)))
    (assignment (name-expr y) (name-expr u))
    (local-signal u (register-expr (literal-expr 0) (name-expr s)))
    (local-signal s (register-expr (literal-expr 0) (name-expr x)))))

(define c33-inst (C33))

(test-case "Can infer types when assignments are in reverse order"
  (check-equal? (minimize (slot-type (dict-ref c33-inst 's))) (minimize (slot-type (dict-ref c33-inst 'x))))
  (check-equal? (minimize (slot-type (dict-ref c33-inst 'u))) (minimize (slot-type (dict-ref c33-inst 's)))))

(begin-hydromel
  (component C34
    (data-port x in  (call-expr array (literal-expr 4) (call-expr unsigned (literal-expr 8))))
    (data-port i in  (call-expr unsigned (literal-expr 2)))
    (data-port y out (call-expr unsigned (literal-expr 8)))
    (assignment (name-expr y) (indexed-array-expr (name-expr x) (name-expr i)))))

(define c34-inst (C34))
(instance-set! c34-inst 'x (signal (pvector 10 20 30 40)))
(instance-set! c34-inst 'i (signal 0 1 2 3))

(test-case "Can read an array"
  (check-sig-equal? (instance-ref c34-inst 'y) (signal 10 20 30 40) 4))

(begin-hydromel
  (component C35
    (data-port y out (call-expr array (literal-expr 3) (call-expr unsigned (literal-expr 8))))
    (assignment (name-expr y) (array-expr (literal-expr 10) (literal-expr 20) (literal-expr 30))))

  (define c35-inst (C35))

  (test-case "Can make a vector"
    (check-sig-equal? (instance-ref c35-inst 'y) (signal (pvector 10 20 30)) 1)))

(begin-hydromel
  (component C36
    (data-port x in (call-expr unsigned (literal-expr 8)))
    (data-port y out (call-expr array (literal-expr 3) (call-expr unsigned (literal-expr 9))))
    (assignment (name-expr y)
      (array-for-expr (add-expr (name-expr x) + (name-expr i))
                      i (range-expr (literal-expr 1) .. (literal-expr 3))))))

(define c36-inst (C36))
(instance-set! c36-inst 'x (signal 10 20 30))

(test-case "Can make a vector comprehension"
  (check-sig-equal? (instance-ref c36-inst 'y) (signal #(11 12 13) #(21 22 23) #(31 32 33)) 3))

(begin-hydromel
  (component C37
    (data-port x in  (call-expr unsigned (literal-expr 4)))
    (data-port y out (call-expr unsigned (literal-expr 4)))
    (assignment (name-expr y)
      (concat-for-expr (slice-expr (name-expr x) (name-expr i))
                       i (range-expr (literal-expr 0) .. (literal-expr 3))))))

(define c37-inst (C37))
(instance-set! c37-inst 'x (signal 10 11 12))

(test-case "Can make a slice comprehension"
  (check-sig-equal? (instance-ref c37-inst 'y) (signal 5 13 3) 3))

(begin-hydromel
  (interface I6
    (data-port z in (call-expr unsigned (literal-expr 1))))

  (component C38
    (composite-port x ((literal-expr 4)) I6)
    (data-port y out (call-expr unsigned (literal-expr 4)))
    (assignment (name-expr y)
      (concat-for-expr (field-expr (indexed-port-expr (name-expr x) (name-expr i)) z)
                       i (range-expr (literal-expr 3) .. (literal-expr 0))))))

(define c38-inst (C38))
(instance-set! c38-inst '(x 0 z) (signal 1 0 0))
(instance-set! c38-inst '(x 1 z) (signal 1 1 0))
(instance-set! c38-inst '(x 2 z) (signal 0 1 1))
(instance-set! c38-inst '(x 3 z) (signal 0 0 1))

(test-case "Can make a slice comprehension using an array composite port"
  (check-sig-equal? (instance-ref c38-inst 'y) (signal 3 6 12) 3))

(begin-hydromel
  (component C39
    (composite-port x ((literal-expr 4)) I6)
    (data-port y out (call-expr array (literal-expr 4) (call-expr unsigned (literal-expr 1))))
    (assignment (name-expr y)
      (array-for-expr (field-expr (indexed-port-expr (name-expr x) (name-expr i)) z)
                      i (range-expr (literal-expr 3) .. (literal-expr 0))))))

(define c39-inst (C39))
(instance-set! c39-inst '(x 0 z) (signal 1 0 0))
(instance-set! c39-inst '(x 1 z) (signal 1 1 0))
(instance-set! c39-inst '(x 2 z) (signal 0 1 1))
(instance-set! c39-inst '(x 3 z) (signal 0 0 1))

(test-case "Can make an array comprehension using an array composite port"
  (check-sig-equal? (instance-ref c39-inst 'y) (signal #(0 0 1 1) #(0 1 1 0) #(1 1 0 0)) 3))

(begin-hydromel
  (component C40
    (composite-port x ((literal-expr 4)) I6)
    (data-port y out (call-expr array (literal-expr 12) (call-expr unsigned (literal-expr 3))))
    (assignment (name-expr y)
      (array-for-expr (add-expr
                        (field-expr (indexed-port-expr (name-expr x) (name-expr i)) z) +
                        (name-expr j))
                      i (range-expr (literal-expr 3) .. (literal-expr 0))
                      j (range-expr (literal-expr 2) .. (literal-expr 0))))))

(define c40-inst (C40))
(instance-set! c40-inst '(x 0 z) (signal 1 0 0))
(instance-set! c40-inst '(x 1 z) (signal 1 1 0))
(instance-set! c40-inst '(x 2 z) (signal 0 1 1))
(instance-set! c40-inst '(x 3 z) (signal 0 0 1))

(test-case "Can make a 2D array comprehension using an array composite port"
  (check-sig-equal? (instance-ref c40-inst 'y) (signal #(2 1 0 2 1 0 3 2 1 3 2 1) #(2 1 0 3 2 1 3 2 1 2 1 0) #(3 2 1 3 2 1 2 1 0 2 1 0)) 3))

(begin-hydromel
  (component C43
    (composite-port x ((literal-expr 4)) I6)
    (data-port y out (call-expr array (literal-expr 10) (call-expr unsigned (literal-expr 3))))
    (assignment (name-expr y)
      (array-for-expr (add-expr
                        (field-expr (indexed-port-expr (name-expr x) (name-expr i)) z) +
                        (name-expr j))
                      i (range-expr (literal-expr 3) .. (literal-expr 0))
                      j (range-expr (name-expr i)    .. (literal-expr 0))))))

(define c43-inst (C43))
(instance-set! c43-inst '(x 0 z) (signal 1 0 0))
(instance-set! c43-inst '(x 1 z) (signal 1 1 0))
(instance-set! c43-inst '(x 2 z) (signal 0 1 1))
(instance-set! c43-inst '(x 3 z) (signal 0 0 1))

(test-case "Can make a 2D array comprehension with index dependencies using an array composite port"
  (check-sig-equal? (instance-ref c43-inst 'y) (signal #(3 2 1 0 2 1 0 2 1 1) #(3 2 1 0 3 2 1 2 1 0) #(4 3 2 1 3 2 1 1 0 0)) 3))

(begin-hydromel
  (typedef word (call-expr unsigned (literal-expr 32)))
  (typedef utwice (parameter n (call-expr natural)) (call-expr unsigned (mult-expr (name-expr n) * (literal-expr 2))))

  (component C42
    (data-port x in (call-expr word))
    (data-port y out (call-expr utwice (literal-expr 16)))
    (assignment (name-expr y) (name-expr x))))

(define c42-inst (C42))

(test-case "Can declare module-level types"
  (check-equal? (slot-type (instance-ref* c42-inst 'x)) (unsigned-type 32))
  (check-equal? (slot-type (instance-ref* c42-inst 'y)) (unsigned-type 32)))

(begin-hydromel
  (component C41
    (data-port s in (call-expr unsigned (literal-expr 3)))
    (data-port a in (call-expr unsigned (literal-expr 8)))
    (data-port b in (call-expr unsigned (literal-expr 8)))
    (data-port c in (call-expr unsigned (literal-expr 8)))
    (data-port y out (call-expr unsigned (literal-expr 8)))
    (assignment (name-expr y)
      (case-expr (name-expr s)
        (choices (literal-expr 0) (literal-expr 2)) (name-expr a)
        (choices (literal-expr 1) (literal-expr 3)) (name-expr b)
                                                    (name-expr c)))))

(define c41-inst (C41))
(instance-set! c41-inst 's (signal 0 1 2 3 4))
(instance-set! c41-inst 'a (signal 10))
(instance-set! c41-inst 'b (signal 20))
(instance-set! c41-inst 'c (signal 30))

(test-case "Can use case expression"
  (check-sig-equal? (instance-ref c41-inst 'y) (signal 10 20 10 20 30) 5))

(begin-hydromel
  (component C44
    (composite-port x ((literal-expr 2) (literal-expr 3)) I6)
    (composite-port y ((literal-expr 2) (literal-expr 3)) flip I6)
    (for-statement i (range-expr (literal-expr 0) .. (literal-expr 1))
      (statement-block
        (for-statement j (range-expr (literal-expr 0) .. (literal-expr 2))
          (statement-block
            (assignment (field-expr (indexed-port-expr (name-expr y) (name-expr i) (name-expr j)) z)
                        (prefix-expr not (field-expr (indexed-port-expr (name-expr x) (name-expr i) (name-expr j)) z)))))))))

(define c44-inst (C44))

(instance-set! c44-inst '(x 0 0 z) (signal 1 0 0))
(instance-set! c44-inst '(x 0 1 z) (signal 1 1 0))
(instance-set! c44-inst '(x 0 2 z) (signal 0 1 1))
(instance-set! c44-inst '(x 1 0 z) (signal 0 0 1))
(instance-set! c44-inst '(x 1 1 z) (signal 1 0 1))
(instance-set! c44-inst '(x 1 2 z) (signal 1 1 1))

(test-case "Can use multidimensional composite ports"
  (check-sig-equal? (instance-ref c44-inst '(y 0 0 z)) (signal 0 1 1) 3)
  (check-sig-equal? (instance-ref c44-inst '(y 0 1 z)) (signal 0 0 1) 3)
  (check-sig-equal? (instance-ref c44-inst '(y 0 2 z)) (signal 1 0 0) 3)
  (check-sig-equal? (instance-ref c44-inst '(y 1 0 z)) (signal 1 1 0) 3)
  (check-sig-equal? (instance-ref c44-inst '(y 1 1 z)) (signal 0 1 0) 3)
  (check-sig-equal? (instance-ref c44-inst '(y 1 2 z)) (signal 0 0 0) 3))
