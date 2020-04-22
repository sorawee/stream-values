#lang racket

(require stream-values
         racket/generator)

(define lim 1000000)

(define-syntax-rule (do-test x)
  (begin (collect-garbage)
         (collect-garbage)
         (collect-garbage)
         (void (x))
         (newline)))

(define (test-native-stream)
  (define s (for/stream ([i (in-range lim)]) i))
  (time (for/sum ([a (in-stream s)]) a))
  (time (for/sum ([a (in-stream s)]) a)))

(define (test-in-stream)
  (define s (for/stream/values ([i (in-range lim)])
              (values i (add1 i))))

  (time (for/sum ([(a b) (in-stream s)]) (+ a b)))
  (time (for/sum ([(a b) (in-stream s)]) (+ a b))))

(define (test-in-generator)
  (define s
    (in-generator
     #:arity 2
     (for ([i (in-range lim)])
       (yield i (add1 i)))))

  (time (for/sum ([(a b) s]) (+ a b)))
  (time (for/sum ([(a b) s]) (+ a b))))

(define (test-unsafe-in-stream)
  (define s (for/stream/values ([i (in-range lim)])
              (values i (add1 i))))

  (time (for/sum ([(a b) (unsafe-in-stream s)]) (+ a b)))
  (time (for/sum ([(a b) (unsafe-in-stream s)]) (+ a b))))

(define (test-unsafe-in-stream-memoized)
  (define s (for/stream/values ([i (in-range lim)])
              (values i (add1 i))))

  (time (for/sum ([(a b) (in-stream s)]) (+ a b)))
  (time (for/sum ([(a b) (unsafe-in-stream s)]) (+ a b))))


(do-test test-native-stream)
(do-test test-in-stream)
(do-test test-in-generator)
(do-test test-unsafe-in-stream)
(do-test test-unsafe-in-stream-memoized)

#|
Racket BC:

cpu time: 639 real time: 691 gc time: 462
cpu time: 115 real time: 124 gc time: 0

cpu time: 1132 real time: 1139 gc time: 715
cpu time: 234 real time: 236 gc time: 0

cpu time: 3355 real time: 3379 gc time: 99
cpu time: 1 real time: 0 gc time: 0

cpu time: 100 real time: 101 gc time: 0
cpu time: 102 real time: 104 gc time: 1

cpu time: 1076 real time: 1083 gc time: 639
cpu time: 322 real time: 324 gc time: 0


Racket CS:

cpu time: 552 real time: 558 gc time: 390
cpu time: 103 real time: 107 gc time: 0

cpu time: 615 real time: 623 gc time: 311
cpu time: 274 real time: 302 gc time: 0

cpu time: 538 real time: 550 gc time: 4
cpu time: 0 real time: 0 gc time: 0

cpu time: 61 real time: 63 gc time: 1
cpu time: 62 real time: 65 gc time: 0

cpu time: 604 real time: 611 gc time: 309
cpu time: 551 real time: 563 gc time: 0
|#
