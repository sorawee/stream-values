#lang info
(define collection "stream-values")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/stream-values.scrbl" ())))
(define pkg-desc "A library for multiple values manipulation in streams")
(define version "0.0")
(define pkg-authors '(sorawee))
(define license '(Apache-2.0 OR MIT))
