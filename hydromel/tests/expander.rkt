; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  rackunit
  (only-in data/collection
    nth)
  data/pvector
  "../lib/expander.rkt"
  "../lib/signal.rkt"
  "../lib/std.rkt"
  "../lib/types.rkt"
  "../lib/slot.rkt"
  "../lib/helpers.rkt")

(define (check-sig-equal? t e n)
  (check-equal? (signal-take t n) (signal-take e n)))


(interface I0
  (parameter N (call-expr unsigned (literal-expr 32)))
  (data-port x in  (call-expr signed (name-expr N)))
  (data-port y out (call-expr signed (name-expr N))))

(define i0-inst (I0-make 30))

(test-case "Can construct a channel for an interface with simple ports"
  (check-pred slot? (dict-ref i0-inst 'x))
  (check-pred slot? (dict-ref i0-inst 'y)))

(component C0
  (parameter N (call-expr unsigned (literal-expr 32)))
  (data-port x in  (call-expr signed (name-expr N)))
  (data-port y out (call-expr signed (name-expr N)))
  (assignment (name-expr y) (slot-expr (name-expr x))))

(define c0-inst (C0-make 30))
(slot-set! (c0-inst x) (signal 23))

(test-case "Can construct a channel for a component with simple ports"
  (check-pred slot? (dict-ref c0-inst 'x))
  (check-pred slot? (dict-ref c0-inst 'y)))

(test-case "Can assign a simple port to another simple port"
  (check-sig-equal? (slot-ref c0-inst y) (slot-ref c0-inst x) 5))

(test-case "Can infer the type of a port"
  (check-equal? (slot-type (dict-ref c0-inst 'x)) (signed 30)))

(interface I1
  (data-port x in  (call-expr signed (literal-expr 32)))
  (data-port y out (call-expr signed (literal-expr 32))))

(interface I2
  (data-port z in (call-expr signed (literal-expr 32)))
  (composite-port i () I1)
  (data-port u out (call-expr signed (literal-expr 32))))

(interface I3
  (data-port v in (call-expr signed (literal-expr 32)))
  (composite-port j () I2)
  (data-port w out (call-expr signed (literal-expr 32))))

(define i2-inst (I2-make))
(define i3-inst (I3-make))

(test-case "Can construct a channel for an interface with composite ports and no parameters"
  (check-pred slot? (dict-ref i2-inst 'z))
  (check-pred dict? (dict-ref i2-inst 'i))
  (check-pred slot? (dict-ref (dict-ref i2-inst 'i) 'x))
  (check-pred slot? (dict-ref (dict-ref i2-inst 'i) 'y))
  (check-pred slot? (dict-ref i2-inst 'u))

  (check-pred slot? (dict-ref i3-inst 'v))
  (check-pred dict? (dict-ref i3-inst 'j))
  (check-pred slot? (dict-ref (dict-ref i3-inst 'j) 'z))
  (check-pred slot? (dict-ref (dict-ref (dict-ref i3-inst 'j) 'i) 'x))
  (check-pred slot? (dict-ref (dict-ref (dict-ref i3-inst 'j) 'i) 'y))
  (check-pred slot? (dict-ref (dict-ref i3-inst 'j) 'u))
  (check-pred slot? (dict-ref i3-inst 'w)))

(component C1
  (data-port v in (call-expr signed (literal-expr 32)))
  (composite-port j () I2)
  (data-port w out (call-expr signed (literal-expr 32)))
  (assignment (name-expr w) (slot-expr (name-expr v))))

(define c1-inst (C1-make))

(test-case "Can construct a channel for a component with composite ports and no parameters"
  (check-pred slot? (dict-ref c1-inst 'v))
  (check-pred dict? (dict-ref c1-inst 'j))
  (check-pred slot? (dict-ref (dict-ref c1-inst 'j) 'z))
  (check-pred slot? (dict-ref (dict-ref (dict-ref c1-inst 'j) 'i) 'x))
  (check-pred slot? (dict-ref (dict-ref (dict-ref c1-inst 'j) 'i) 'y))
  (check-pred slot? (dict-ref (dict-ref c1-inst 'j) 'u))
  (check-pred slot? (dict-ref c1-inst 'w)))

(interface I4
  (composite-port i ((literal-expr 3)) I1))

(define i4-inst (I4-make))

(test-case "Can construct a channel for an interface with a vector port"
  (check-pred vector? (dict-ref i4-inst 'i))
  (check-eq? (vector-length (dict-ref i4-inst 'i)) 3)
  (for ([n (range:impl 0 2)])
    (check-pred dict? (vector-ref (dict-ref i4-inst 'i) n))))

(interface I5
  (parameter N (call-expr unsigned (literal-expr 32)))
  (composite-port i ((name-expr N)) I1))

(define i5-inst (I5-make 5))

(test-case "Can construct a channel for an interface with arguments"
  (check-pred vector? (dict-ref i5-inst 'i))
  (check-eq? (vector-length (dict-ref i5-inst 'i)) 5)
  (for ([n (range:impl 0 4)])
    (check-pred dict? (vector-ref (dict-ref i5-inst 'i) n))))

(interface I6
  (parameter M (call-expr unsigned (literal-expr 32)))
  (composite-port j () I5 (name-expr M)))

(define i6-inst (I6-make 3))

(test-case "Can construct a channel containing a composite port with arguments"
  (check-pred vector? (dict-ref (dict-ref i6-inst 'j) 'i))
  (check-eq? (vector-length (dict-ref (dict-ref i6-inst 'j) 'i)) 3)
  (for ([n (range:impl 0 2)])
    (check-pred dict? (vector-ref (dict-ref (dict-ref i6-inst 'j) 'i) n))))

(component C2
  (composite-port i () I1)
  (assignment (field-expr (name-expr i) y)
              (slot-expr (field-expr (name-expr i) x))))

(define c2-inst (C2-make))
(slot-set! (c2-inst i x) (signal 23))

(test-case "Can access simple ports in a composite port"
  (check-sig-equal? (slot-ref c2-inst i y) (slot-ref c2-inst i x) 5))

(component C3
  (composite-port i ((literal-expr 3)) I1)
  (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 0)) y)
              (slot-expr (field-expr (indexed-port-expr (name-expr i) (literal-expr 0)) x)))
  (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 1)) y)
              (slot-expr (field-expr (indexed-port-expr (name-expr i) (literal-expr 1)) x)))
  (assignment (field-expr (indexed-port-expr (name-expr i) (literal-expr 2)) y)
              (slot-expr (field-expr (indexed-port-expr (name-expr i) (literal-expr 2)) x))))

(define c3-inst (C3-make))
(slot-set! (c3-inst i 0 x) (signal 10))
(slot-set! (c3-inst i 1 x) (signal 20))
(slot-set! (c3-inst i 2 x) (signal 30))

(test-case "Can access simple ports in a vector composite port with static indices"
  (check-sig-equal? (slot-ref c3-inst i 0 y) (slot-ref c3-inst i 0 x) 5)
  (check-sig-equal? (slot-ref c3-inst i 1 y) (slot-ref c3-inst i 1 x) 5)
  (check-sig-equal? (slot-ref c3-inst i 2 y) (slot-ref c3-inst i 2 x) 5))

(interface I7
  (data-port x in (call-expr signed (literal-expr 32))))

(component C4
  (composite-port i ((literal-expr 3)) I7)
  (data-port y in (call-expr signed (literal-expr 32)))
  (data-port z out (call-expr signed (literal-expr 32)))
  (assignment (name-expr z)
              (lift-expr [y^ (slot-expr (name-expr y))]
                         (slot-expr (field-expr (indexed-port-expr (name-expr i) (name-expr y^)) x)))))

(define c4-inst (C4-make))
(slot-set! (c4-inst i 0 x) (signal 10))
(slot-set! (c4-inst i 1 x) (signal 20))
(slot-set! (c4-inst i 2 x) (signal 30))
(slot-set! (c4-inst y)     (signal 0 1 2 1 0 2))

(test-case "Can access simple ports in a vector composite port with dynamic indices"
  (check-sig-equal? (slot-ref c4-inst z) (signal 10 20 30 20 10 30) 6))

(component C5
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y in (call-expr signed (literal-expr 32)))
  (data-port z out (call-expr signed (literal-expr 32)))
  (assignment (name-expr z)
    (lift-expr [x^ (slot-expr (name-expr x))]
               [y^ (slot-expr (name-expr y))]
      (call-expr cast:impl
        (call-expr signed (literal-expr 32))
        (call-expr + (name-expr x^) (name-expr y^))))))

(define c5-inst (C5-make))
(slot-set! (c5-inst x) (signal 1  2  3  4  5))
(slot-set! (c5-inst y) (signal 10 20 30 40 50))

(test-case "Can perform an operation between signals"
  (check-sig-equal? (slot-ref c5-inst z) (signal 11 22 33 44 55) 5))

(component C6
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y in (call-expr signed (literal-expr 32)))
  (data-port z in (call-expr signed (literal-expr 32)))
  (data-port u in (call-expr signed (literal-expr 32)))
  (data-port v out (call-expr signed (literal-expr 32)))
  (local-signal xy (lift-expr [x^ (slot-expr (name-expr x))]
                              [y^ (slot-expr (name-expr y))]
                              (call-expr * (name-expr x^) (name-expr y^))))
  (local-signal zu (lift-expr [z^ (slot-expr (name-expr z))]
                              [u^ (slot-expr (name-expr u))]
                              (call-expr * (name-expr z^) (name-expr u^))))
  (assignment (name-expr v)
    (lift-expr [xy^ (slot-expr (name-expr xy))]
               [zu^ (slot-expr (name-expr zu))]
      (call-expr cast:impl
        (call-expr signed (literal-expr 32))
        (call-expr + (name-expr xy^) (name-expr zu^))))))

(define c6-inst (C6-make))
(slot-set! (c6-inst x) (signal 10 20 30 40 50))
(slot-set! (c6-inst y) (signal 2))
(slot-set! (c6-inst z) (signal 1 2 3 4 5))
(slot-set! (c6-inst u) (signal 3))

(test-case "Can use local signals"
  (check-sig-equal? (slot-ref c6-inst v) (signal 23 46 69 92 115) 5))

(component C7
  (parameter N (call-expr unsigned (literal-expr 32)))
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y out (call-expr signed (literal-expr 32)))
  (assignment (name-expr y)
              (lift-expr [x^ (slot-expr (name-expr x))]
                (call-expr cast:impl
                  (call-expr signed (literal-expr 32))
                  (call-expr * (name-expr x^) (name-expr N))))))

(component C8
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y out (call-expr signed (literal-expr 32)))
  (instance c () C7 (literal-expr 10))
  (assignment (field-expr (name-expr c) x) (slot-expr (name-expr x)))
  (assignment (name-expr y) (slot-expr (field-expr (name-expr c) y))))

(define c8-inst (C8-make))
(slot-set! (c8-inst x) (signal 10 20 30 40 50))

(test-case "Can instantiate a component"
  (check-sig-equal? (slot-ref c8-inst y) (signal 100 200 300 400 500) 5))

(component C9
  (data-port x0 in (call-expr signed (literal-expr 32)))
  (data-port x1 in (call-expr signed (literal-expr 32)))
  (data-port y out (call-expr signed (literal-expr 32)))
  (instance c ((literal-expr 2)) C7 (literal-expr 10))
  (assignment (field-expr (indexed-port-expr (name-expr c) (literal-expr 0)) x) (slot-expr (name-expr x0)))
  (assignment (field-expr (indexed-port-expr (name-expr c) (literal-expr 1)) x) (slot-expr (name-expr x1)))
  (assignment (name-expr y)
    (lift-expr [y0 (slot-expr (field-expr (indexed-port-expr (name-expr c) (literal-expr 0)) y))]
               [y1 (slot-expr (field-expr (indexed-port-expr (name-expr c) (literal-expr 1)) y))]
      (call-expr cast:impl
        (call-expr signed (literal-expr 32))
        (call-expr + (name-expr y0) (name-expr y1))))))

(define c9-inst (C9-make))
(slot-set! (c9-inst x0) (signal 10 20 30 40 50))
(slot-set! (c9-inst x1) (signal 1 2 3 4 5))

(test-case "Can instantiate a multiple component"
  (check-sig-equal? (slot-ref c9-inst y) (signal 110 220 330 440 550) 5))

(component C10
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y out (call-expr signed (literal-expr 32)))
  (assignment (name-expr y) (register-expr (literal-expr 0) (slot-expr (name-expr x)))))

(define c10-inst (C10-make))
(slot-set! (c10-inst x) (signal 10 20 30 40 50))

(test-case "Can register a signal"
  (check-sig-equal? (slot-ref c10-inst y) (signal 0  10 20 30 40 50) 6))

(component C11
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y in (call-expr signed (literal-expr 32)))
  (data-port z out (call-expr signed (literal-expr 32)))
  (assignment (name-expr z) (register-expr (literal-expr 0) (when-clause (slot-expr (name-expr x)))
                                           (slot-expr (name-expr y)))))

(define c11-inst (C11-make))
(slot-set! (c11-inst x) (signal #f #f #f #t #f))
(slot-set! (c11-inst y) (signal 10 20 30 40 50))

(test-case "Can register a signal with reset"
  (check-sig-equal? (slot-ref c11-inst z) (signal 0  10 20 30 0  50) 6))

(component C12
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y in (call-expr signed (literal-expr 32)))
  (data-port z out (call-expr signed (literal-expr 32)))
  (assignment (name-expr z) (register-expr (literal-expr 0)
                                           (slot-expr (name-expr y)) (when-clause (slot-expr (name-expr x))))))

(define c12-inst (C12-make))
(slot-set! (c12-inst x) (signal #f #t #f #t #f))
(slot-set! (c12-inst y) (signal 10 20 30 40 50))

(test-case "Can register a signal with enable"
  (check-sig-equal? (slot-ref c12-inst z) (signal 0  0  20 20 40) 6))

(component C13
  (data-port x in (call-expr signed (literal-expr 32)))
  (data-port y in (call-expr signed (literal-expr 32)))
  (data-port z in (call-expr signed (literal-expr 32)))
  (data-port u out (call-expr signed (literal-expr 32)))
  (assignment (name-expr u)
    (register-expr (literal-expr 0) (when-clause (slot-expr (name-expr x)))
                   (slot-expr (name-expr z)) (when-clause (slot-expr (name-expr y))))))

(define c13-inst (C13-make))
(slot-set! (c13-inst x) (signal #f #f #t #f #f))
(slot-set! (c13-inst y) (signal #f #t #f #t #f))
(slot-set! (c13-inst z) (signal 10 20 30 40 50))

(test-case "Can register a signal with reset and enable"
  (check-sig-equal? (slot-ref c13-inst u) (signal 0  0  20 0  40) 6))

(component C14
  (constant N (literal-expr 56))
  (data-port y out (call-expr signed (literal-expr 32)))
  (assignment (name-expr y) (signal-expr (name-expr N))))

(define c14-inst (C14-make))

(test-case "Can read a local constant"
  (check-sig-equal? (slot-ref c14-inst y) (signal 56) 1))

(test-case "Can read a constant as a channel field"
  (check-equal? (slot-ref c14-inst N) 56))

(interface I8
  (constant N (literal-expr 56))
  (data-port y out (call-expr signed (literal-expr 32))))

(component C15
  (composite-port p () I8)
  (assignment (field-expr (name-expr p) y) (signal-expr (field-expr (name-expr p) N))))

(define c15-inst (C15-make))

(test-case "Can read a constant from a port"
  (check-sig-equal? (slot-ref c15-inst p y) (signal 56) 1))

(component C16
  (data-port y out (call-expr signed (literal-expr 32)))
  (instance c () C14)
  (assignment (name-expr y) (signal-expr (field-expr (name-expr c) N))))

(define c16-inst (C16-make))

(test-case "Can read a constant from an instance"
  (check-sig-equal? (slot-ref c16-inst y) (signal 56) 1))

(component C17
  (data-port y out (call-expr signed (literal-expr 32)))
  (instance c () C15)
  (assignment (name-expr y) (signal-expr (field-expr (field-expr (name-expr c) p) N))))

(define c17-inst (C17-make))

(test-case "Can read a constant from an instance port"
  (check-sig-equal? (slot-ref c17-inst y) (signal 56) 1))

(constant K0 (literal-expr 44))

(component C18
  (data-port y out (call-expr signed (literal-expr 32)))
  (assignment (name-expr y) (signal-expr (name-expr K0 -constant))))

(define c18-inst (C18-make))

(test-case "Can read a global constant"
  (check-sig-equal? (slot-ref c18-inst y) (signal 44) 1))

(component C19
  (constant N (literal-expr 255))
  (data-port x in (call-expr signed (literal-expr 32)))
  (local-signal y (name-expr x))
  (local-signal z (name-expr N)))

(define c19-inst (C19-make))

(test-case "Can infer the type of a local signal that copies a port"
  (check-equal? (slot-type (dict-ref c19-inst 'y)) (slot-type (dict-ref c19-inst 'x))))

(test-case "Can infer the type of a local signal that copies a constant"
  (check-equal? (slot-type (dict-ref c19-inst 'y)) (slot-type (dict-ref c19-inst 'x)))
  (check-equal? (slot-type (dict-ref c19-inst 'z)) (static-data 255 (unsigned 8))))

(component C20
  (data-port x in  (call-expr signed (literal-expr 4)))
  (data-port y in  (call-expr signed (literal-expr 4)))
  (data-port z out (call-expr signed (literal-expr 8)))
  (assignment (name-expr z)
    (lift-expr [x^ (slot-expr (name-expr x))]
               [y^ (slot-expr (name-expr y))]
      (call-expr/cast concat:impl (name-expr x^) (call-expr signed (literal-expr 4))
                                  (name-expr y^) (call-expr signed (literal-expr 4))))))

(define c20-inst (C20-make))
(slot-set! (c20-inst x) (signal 0 5 -2))
(slot-set! (c20-inst y) (signal 0 3 -4))

(test-case "Can concatenate two integers"
  (check-sig-equal? (slot-ref c20-inst z) (signal 0 83 -20) 3))

(component C21
  (data-port x in  (call-expr unsigned (literal-expr 8)))
  (data-port y out (call-expr unsigned (literal-expr 8)))
  (assignment (name-expr y) (slot-expr (name-expr u)))
  (local-signal u (register-expr (literal-expr 0) (slot-expr (name-expr s))))
  (local-signal s (register-expr (literal-expr 0) (slot-expr (name-expr x)))))

(define c21-inst (C21-make))

(test-case "Can infer types when assignments are in reverse order"
  (check-equal? (normalize-type (slot-type (dict-ref c21-inst 's))) (normalize-type (slot-type (dict-ref c21-inst 'x))))
  (check-equal? (normalize-type (slot-type (dict-ref c21-inst 'u))) (normalize-type (slot-type (dict-ref c21-inst 's)))))

(component C22
  (data-port x in  (call-expr array (literal-expr 4) (call-expr unsigned (literal-expr 8))))
  (data-port i in  (call-expr unsigned (literal-expr 2)))
  (data-port y out (call-expr unsigned (literal-expr 8)))
  (assignment (name-expr y) (lift-expr [x^ (slot-expr (name-expr x))]
                                       [i^ (slot-expr (name-expr i))]
                              (call-expr nth (name-expr x^) (name-expr i^)))))

(define c22-inst (C22-make))
(slot-set! (c22-inst x) (signal (pvector 10 20 30 40)))
(slot-set! (c22-inst i) (signal 0 1 2 3))

(test-case "Can read an array"
  (check-sig-equal? (slot-ref c22-inst y) (signal 10 20 30 40) 4))

(component C23
  (data-port y out (call-expr array (literal-expr 3) (call-expr unsigned (literal-expr 8))))
  (assignment (name-expr y) (signal-expr (call-expr pvector (literal-expr 10) (literal-expr 20) (literal-expr 30)))))

(define c23-inst (C23-make))

(test-case "Can make a vector"
  (check-sig-equal? (slot-ref c23-inst y) (signal (pvector 10 20 30)) 1))

(component C24
  (data-port x in (call-expr unsigned (literal-expr 8)))
  (data-port y out (call-expr array (literal-expr 3) (call-expr unsigned (literal-expr 9))))
  (assignment (name-expr y)
    (lift-expr [x^ (slot-expr (name-expr x))]
      (array-for-expr
        (call-expr + (name-expr x^) (name-expr i))
        i (call-expr range:impl (literal-expr 1) (literal-expr 3))))))

(define c24-inst (C24-make))
(slot-set! (c24-inst x) (signal 10 20 30))

(test-case "Can make a vector comprehension"
  (check-sig-equal? (slot-ref c24-inst y) (signal (pvector 11 12 13) (pvector 21 22 23) (pvector 31 32 33)) 3))

(component C25
  (data-port x in  (call-expr unsigned (literal-expr 4)))
  (data-port y out (call-expr unsigned (literal-expr 4)))
  (assignment (name-expr y)
    (lift-expr [x^ (slot-expr (name-expr x))]
      (concat-for-expr
        (call-expr unsigned-slice (name-expr x^) (name-expr i) (name-expr i))
        i (call-expr range:impl (literal-expr 0) (literal-expr 3))))))

(define c25-inst (C25-make))
(slot-set! (c25-inst x) (signal 10 11 12))

(test-case "Can make a slice comprehension"
  (check-sig-equal? (slot-ref c25-inst y) (signal 5 13 3) 3))

(interface I9
  (data-port z in (call-expr unsigned (literal-expr 1))))

(component C26
  (composite-port x ((literal-expr 4)) I9)
  (data-port y out (call-expr unsigned (literal-expr 4)))
  (assignment (name-expr y)
    (concat-for-expr
      (lift-expr [x^ (slot-expr (field-expr (indexed-port-expr (name-expr x) (name-expr i)) z))]
        (name-expr x^))
      i (call-expr range:impl (literal-expr 3) (literal-expr 0)))))

(define c26-inst (C26-make))
(slot-set! (c26-inst x 0 z) (signal 1 0 0))
(slot-set! (c26-inst x 1 z) (signal 1 1 0))
(slot-set! (c26-inst x 2 z) (signal 0 1 1))
(slot-set! (c26-inst x 3 z) (signal 0 0 1))

(test-case "Can make a slice comprehension using an array composite port"
  (check-sig-equal? (slot-ref c26-inst y) (signal 3 6 12) 3))

(component C27
  (composite-port x ((literal-expr 4)) I9)
  (data-port y out (call-expr array (literal-expr 4) (call-expr unsigned (literal-expr 1))))
  (assignment (name-expr y)
    (array-for-expr
      (lift-expr [x^ (slot-expr (field-expr (indexed-port-expr (name-expr x) (name-expr i)) z))]
        (name-expr x^))
      i (call-expr range:impl (literal-expr 3) (literal-expr 0)))))

(define c27-inst (C27-make))
(slot-set! (c27-inst x 0 z) (signal 1 0 0))
(slot-set! (c27-inst x 1 z) (signal 1 1 0))
(slot-set! (c27-inst x 2 z) (signal 0 1 1))
(slot-set! (c27-inst x 3 z) (signal 0 0 1))

(test-case "Can make an array comprehension using an array composite port"
  (check-sig-equal? (slot-ref c27-inst y) (signal #(0 0 1 1) #(0 1 1 0) #(1 1 0 0)) 3))

(typedef word (call-expr unsigned (literal-expr 32)))
(typedef utwice (parameter n (call-expr natural:impl)) (call-expr unsigned (call-expr * (name-expr n) (literal-expr 2))))

(component C28
  (data-port x in (call-expr word:impl))
  (data-port y out (call-expr utwice:impl (literal-expr 16)))
  (assignment (name-expr y) (slot-expr (name-expr x))))

(define c28-inst (C28-make))

(test-case "Can declare module-level types"
  (check-equal? (slot-type (slot-ref* c28-inst x)) (unsigned 32))
  (check-equal? (slot-type (slot-ref* c28-inst y)) (unsigned 32)))

; TODO test multidimensional ports
; TODO test multidimensional arrays
; TODO test multidimensional concat comprehensions
; TODO test multidimensional array comprehensions
