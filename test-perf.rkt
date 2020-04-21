#lang racket

(require stream-values
         racket/generator)

(define lim 1000000)

(define (test-native-stream)
  (define s (for/stream ([i (in-range lim)]) i))
  (for/sum ([a (in-stream s)]) a))

(define (test-in-stream)
  (define s (for/stream/values ([i (in-range lim)])
              (values i (add1 i))))

  (for/sum ([(a b) (in-stream s)]) (+ a b)))

(define (test-in-generator)
  (define s
    (in-generator
     #:arity 2
     (for ([i (in-range lim)])
       (yield i (add1 i)))))

  (for/sum ([(a b) s]) (+ a b)))

(define (test-unsafe-in-stream)
  (define s (for/stream/values ([i (in-range lim)])
              (values i (add1 i))))

  (for/sum ([(a b) (unsafe-in-stream s)]) (+ a b)))


(time (test-native-stream))
(time (test-in-stream))
(time (test-in-generator))
(time (test-unsafe-in-stream))

#|
Racket BC:
cpu time: 451 real time: 457 gc time: 286
499999500000
cpu time: 831 real time: 834 gc time: 385
1000000000000
cpu time: 3352 real time: 3390 gc time: 67
1000000000000
cpu time: 94 real time: 97 gc time: 1
1000000000000

Racket CS:

cpu time: 469 real time: 491 gc time: 304
499999500000
cpu time: 660 real time: 677 gc time: 320
1000000000000
cpu time: 625 real time: 657 gc time: 80
1000000000000
cpu time: 71 real time: 74 gc time: 0
1000000000000
|#
