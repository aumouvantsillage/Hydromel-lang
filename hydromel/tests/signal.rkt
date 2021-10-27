; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.

#lang racket

(require
  "../lib/signal.rkt"
  rackunit)

(letrec ([s (signal-cons 42 s)])
  (check-pred signal? s)
  (check-equal? (signal-first s) 42)
  (check-equal? (signal-rest s) s)
  (check-equal? (signal-take s 3) (list 42 42 42)))

(let ([s (list->signal (list 10 20 30))])
  (check-pred signal? s)
  (check-equal? (signal-first s) 10)
  (check-pred signal? (signal-rest s))
  (check-equal? (signal-first (signal-rest s)) 20)
  (check-equal? (signal-take s 5) (list 10 20 30 30 30)))

(let ([s (signal 42)])
  (check-equal? (signal-take s 3) (list 42 42 42)))

(let ([s (signal 10 20 30)])
  (check-equal? (signal-take s 5) (list 10 20 30 30 30)))

(let ()
  (define-signal s 42)
  (check-equal? (signal-take s 3) (list 42 42 42)))

(let ()
  (define-signal s 10 20 30)
  (check-equal? (signal-take s 5) (list 10 20 30 30 30)))

(let* ([signal+ (signal-lift +)]
       [s1 (signal   1   2   3)]
       [s2 (signal  10  20  30)]
       [s3 (signal 100 200 300)]
       [s4 (signal+ s1 s2 s3)])
  (check-equal? (signal-take s4 3) (list 111 222 333)))

(let* ([signal-if (signal-lift* if _ _ _)]
       [s1 (signal  #f  #t  #f  #t)]
       [s2 (signal  10  20  30  40)]
       [s3 (signal 100 200 300 400)]
       [s4 (signal-if s1 s2 s3)])
  (check-equal? (signal-take s4 4) (list 100 20 300 40)))

(let* ([signal+ (signal-λ lst (apply + lst))]
       [s1 (signal   1   2   3)]
       [s2 (signal  10  20  30)]
       [s3 (signal 100 200 300)]
       [s4 (signal+ s1 s2 s3)])
  (check-equal? (signal-take s4 3) (list 111 222 333)))

(let* ([signal-if (signal-λ (c t e) (if c t e))]
       [s1 (signal  #f  #t  #f  #t)]
       [s2 (signal  10  20  30  40)]
       [s3 (signal 100 200 300 400)]
       [s4 (signal-if s1 s2 s3)])
  (check-equal? (signal-take s4 4) (list 100 20 300 40)))

(let ()
  (define-signal (signal+ . lst)
    (apply + lst))
  (define s1 (signal   1   2   3))
  (define s2 (signal  10  20  30))
  (define s3 (signal 100 200 300))
  (define s4 (signal+ s1 s2 s3))
  (check-equal? (signal-take s4 3) (list 111 222 333)))

(let ()
  (define-signal (signal-if c t e)
    (if c t e))
  (define s1 (signal  #f  #t  #f  #t))
  (define s2 (signal  10  20  30  40))
  (define s3 (signal 100 200 300 400))
  (define s4 (signal-if s1 s2 s3))
  (check-equal? (signal-take s4 4) (list 100 20 300 40)))

(let* ([s1 (signal  #f  #t  #f  #t)]
       [s2 (signal  10  20  30  40)]
       [s3 (signal 100 200 300 400)]
       [s4 (for/signal ([c s1] [t s2] [e s3])
             (if c t e))])
  (check-equal? (signal-take s4 4) (list 100 20 300 40)))

(let* ([s1 (signal  #f  #t  #f  #t)]
       [s2 (signal  10  20  30  40)]
       [s3 (signal 100 200 300 400)]
       [s4 (for/signal (s1 s2 s3)
             (if s1 s2 s3))])
  (check-equal? (signal-take s4 4) (list 100 20 300 40)))

(let ([s (register 10 (signal 20 30 40))])
  (check-equal? (signal-take s 4) (list 10 20 30 40)))

(let ([s (register 10 (for/signal (this-reg) (add1 this-reg)))])
  (check-equal? (signal-take s 4) (list 10 11 12 13)))

(let* ([s1 (signal #f #f #t #f)]
       [s2 (register/r 10 s1 (signal 20 30 40 50 60))])
  (check-equal? (signal-take s2 6) (list 10 20 30 10 50 60)))

(let* ([s1 (signal #f #t #f #t #f)]
       [s2 (register/e 10 s1 (signal 20 30 40 50 60))])
  (check-equal? (signal-take s2 6) (list 10 10 30 30 50 50)))

(let* ([s1 (signal #f #f #t #f #f #t #f)]
       [s2 (signal #f #t #f #t #f #t #f)]
       [s3 (register/re 10 s1 s2 (signal 20 30 40 50 60 70))])
  (check-equal? (signal-take s3 7) (list 10 10 30 10 50 50 10)))

(let* ([s1 (signal   1   2   3)]
       [s2 (signal  10  20  30)]
       [s3 (signal 100 200 300)]
       [s4 (signal-bundle s1 s2 s3)])
  (check-equal? (signal-take s4 3) (list (list 1 10 100) (list 2 20 200) (list 3 30 300))))

(let*-values ([(s1)       (signal (list 1 10 100) (list 2 20 200) (list 3 30 300))]
              [(s2 s3 s4) (signal-unbundle s1 3)])
  (check-equal? (signal-take s2 3) (list   1   2   3))
  (check-equal? (signal-take s3 3) (list  10  20  30))
  (check-equal? (signal-take s4 3) (list 100 200 300)))

(let*-values ([(signal-inc3) (signal-λ lst #:returns (x y z)
                               (define x (add1 (first  lst)))
                               (define y (add1 (second lst)))
                               (define z (add1 (third  lst))))]
              [(s1) (signal   1   2   3)]
              [(s2) (signal  10  20  30)]
              [(s3) (signal 100 200 300)]
              [(s4 s5 s6) (signal-inc3 s1 s2 s3)])
  (check-equal? (signal-take s4 3) (list   2   3   4))
  (check-equal? (signal-take s5 3) (list  11  21  31))
  (check-equal? (signal-take s6 3) (list 101 201 301)))

(let*-values ([(signal-inc3) (signal-λ (a b c) #:returns (x y z)
                               (define x (add1 a))
                               (define y (add1 b))
                               (define z (add1 c)))]
              [(s1) (signal   1   2   3)]
              [(s2) (signal  10  20  30)]
              [(s3) (signal 100 200 300)]
              [(s4 s5 s6) (signal-inc3 s1 s2 s3)])
  (check-equal? (signal-take s4 3) (list   2   3   4))
  (check-equal? (signal-take s5 3) (list  11  21  31))
  (check-equal? (signal-take s6 3) (list 101 201 301)))

(let ()
  (define-signal (signal-inc3 . lst) #:returns (x y z)
                   (define x (add1 (first  lst)))
                   (define y (add1 (second lst)))
                   (define z (add1 (third  lst))))
  (define s1 (signal   1   2   3))
  (define s2 (signal  10  20  30))
  (define s3 (signal 100 200 300))
  (define-values (s4 s5 s6) (signal-inc3 s1 s2 s3))
  (check-equal? (signal-take s4 3) (list   2   3   4))
  (check-equal? (signal-take s5 3) (list  11  21  31))
  (check-equal? (signal-take s6 3) (list 101 201 301)))

(let ()
  (define-signal (signal-inc3 a b c) #:returns (x y z)
                   (define x (add1 a))
                   (define y (add1 b))
                   (define z (add1 c)))
  (define s1 (signal   1   2   3))
  (define s2 (signal  10  20  30))
  (define s3 (signal 100 200 300))
  (define-values (s4 s5 s6) (signal-inc3 s1 s2 s3))
  (check-equal? (signal-take s4 3) (list   2   3   4))
  (check-equal? (signal-take s5 3) (list  11  21  31))
  (check-equal? (signal-take s6 3) (list 101 201 301)))

(let*-values ([(s1) (signal   1   2   3)]
              [(s2) (signal  10  20  30)]
              [(s3) (signal 100 200 300)]
              [(s4 s5 s6) (for/signal (s1 s2 s3) #:returns (x y z)
                            (define x (add1 s1))
                            (define y (add1 s2))
                            (define z (add1 s3)))])
  (check-equal? (signal-take s4 3) (list   2   3   4))
  (check-equal? (signal-take s5 3) (list  11  21  31))
  (check-equal? (signal-take s6 3) (list 101 201 301)))

(let ()
  (define signal-inc3 (signal-λ (a #:b b [c (signal 100 200 300)] #:d [d (signal 1000 2000 3000)])
                        #:returns (w x y z)
                        (define w (add1 a))
                        (define x (add1 b))
                        (define y (add1 c))
                        (define z (add1 d))))
  (define s1 (signal   1   2   3))
  (define s2 (signal  10  20  30))
  (define-values (s3 s4 s5 s6) (signal-inc3 s1 #:b s2))
  (check-equal? (signal-take s3 3) (list    2    3    4))
  (check-equal? (signal-take s4 3) (list   11   21   31))
  (check-equal? (signal-take s5 3) (list  101  201  301))
  (check-equal? (signal-take s6 3) (list 1001 2001 3001)))

(let ()
  (define-signal (signal-inc3 a #:b b [c (signal 100 200 300)] #:d [d (signal 1000 2000 3000)])
    #:returns (w x y z)
    (define w (add1 a))
    (define x (add1 b))
    (define y (add1 c))
    (define z (add1 d)))
  (define s1 (signal   1   2   3))
  (define s2 (signal  10  20  30))
  (define-values (s3 s4 s5 s6) (signal-inc3 s1 #:b s2))
  (check-equal? (signal-take s3 3) (list    2    3    4))
  (check-equal? (signal-take s4 3) (list   11   21   31))
  (check-equal? (signal-take s5 3) (list  101  201  301))
  (check-equal? (signal-take s6 3) (list 1001 2001 3001)))
