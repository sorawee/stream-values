#lang scribble/manual
@require[scribble/example
         @for-label[stream-values
                    racket/base
                    racket/stream]]

@(define sequence-evaluator
   (let ([evaluator (make-base-eval)])
     (evaluator '(require stream-values racket/stream))
     evaluator))

@title{stream-values}
@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]

@defmodule[stream-values]

This library allows manipulation of multiple values in @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{streams}.

@deftogether[(@defform[(stream-cons/values first-expr rest-expr)]
              @defform[(stream/values expr ...)]
              @defform[(stream*/values expr ...)]
              @defform[(for/stream/values (for-clause ...) body-or-break ... body)]
              @defform[(for*/stream/values (for-clause ...) body-or-break ... body)])]{
  Like @racket[stream-cons], @racket[stream], @racket[stream*], @racket[for/stream],
  and @racket[for*/stream], but they support @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{multiple values}.

  @examples[#:eval sequence-evaluator
    (define s (stream-cons/values (values 1 2) empty-stream))
    (stream-first s)
    (define t (stream/values (values 1 2) (values 3 4)))
    (for/list ([(left right) (in-stream t)])
      (list left right))
    (define u
      (for/stream/values ([i (in-naturals)])
        (values i (add1 i))))
    (for/list ([(left right) u] [_ 5])
      (list left right))
  ]
}

