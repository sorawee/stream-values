#lang scribble/manual
@require[scribble/example
         racket/function
         @for-label[stream-values
                    racket/base
                    racket/stream]]

@(define sequence-evaluator
   (let ([evaluator (make-base-eval)])
     (evaluator '(require stream-values racket/stream))
     evaluator))

@; from scribblings/reference/sequences.scrbl
@(define (for-element-reachability what)
   @elem{See @racket[for] for information on the reachability of @|what| elements
         during an iteration.})

@(define tech-ref (curry tech #:doc '(lib "scribblings/reference/reference.scrbl")))

@title{stream-values}
@author[@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]]

@defmodule[stream-values]

This library allows manipulation of @tech-ref{multiple values} in @tech-ref{streams}.
The @racket[for/stream/values] form, in particular, could be used to construct a
relatively efficient @tech-ref{sequence} of multiple values in the @tech[#:key "3m" #:doc '(lib "scribblings/guide/guide.scrbl")]{traditional (3m) variant} of Racket (as @tech-ref{generators} are highly inefficient in this variant).

@deftogether[(@defform[(stream-cons/values first-expr rest-expr)]
              @defform[(stream/values expr ...)]
              @defform[(stream*/values expr ...)]
              @defform[(for/stream/values (for-clause ...) body-or-break ... body)]
              @defform[(for*/stream/values (for-clause ...) body-or-break ... body)])]{
  Like @racket[stream-cons], @racket[stream], @racket[stream*], @racket[for/stream],
  and @racket[for*/stream], but they support multiple values.

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

@defproc[(unsafe-in-stream [s stream?]) sequence?]{
  Similar to @racket[in-stream] (which supports multiple values already),
  but it cooperates with this library so that an
  @racket[unsafe-in-stream] application can provide better performance for iteration
  on streams (that are constructed via this library) when @racket[unsafe-in-stream]
  appears directly in a @racket[for] clause. It is @emph{unsafe} in a sense that
  stream memoization (which is a feature of Racket streams) is @emph{not guaranteed}.
  That is, for each element in the stream, an iteration via @racket[unsafe-in-stream]
  might or might not memoize the element. However, it does guarantee that if a stream
  is @emph{fully} memoized, iterating on the stream will use the memoized result, though
  in this case, @racket[in-stream] will provide a better performance.

  This procedure is useful when a stream is used in the iteration only once and then
  discarded, since memoization does not matter and the iteration could be
  significantly faster. On the other hand,
  if the stream will be used again in the future, the lack of memoization could
  result in a performance loss or even a surprisingly incorrect result.

  @for-element-reachability["stream"]
  @examples[#:eval sequence-evaluator
    (code:comment @#,t{Performance of @racket[in-stream] vs @racket[unsafe-in-stream]})
    (define s (for/stream/values ([i (in-range 100000)]) (values i (add1 i))))
    (time (for ([(a b) (in-stream s)]) (void)))
    (define t (for/stream/values ([i (in-range 100000)]) (values i (add1 i))))
    (time (for ([(a b) (unsafe-in-stream t)]) (void)))
  ]
  @examples[#:eval sequence-evaluator #:label #f
    (code:comment @#,t{Lack of memoization})
    (define xs (for/stream/values ([i (in-range 5)]) (displayln i)))
    (for ([_ (in-stream xs)]) (void))
    (code:comment @#,t{This iteration should not display any element because the stream is memoized.})
    (for ([_ (in-stream xs)]) (void))
    (define ys (for/stream/values ([i (in-range 5)]) (displayln i)))
    (for ([_ (unsafe-in-stream ys)]) (void))
    (code:comment @#,t{Due to the lack of memoization, this iteration displays elements again.})
    (for ([_ (unsafe-in-stream ys)]) (void))
  ]
  @examples[#:eval sequence-evaluator #:label #f
    (code:comment @#,t{Fully memoization stream is utilized})
    (define zs (for/stream/values ([i (in-range 5)]) (displayln i)))
    (for ([_ (in-stream zs)]) (void))
    (code:comment @#,t{This iteration should not display any element because the stream is fully memoized.})
    (for ([_ (unsafe-in-stream zs)]) (void))
  ]

  @examples[#:eval sequence-evaluator #:label #f
    (code:comment @#,t{Performance of @racket[in-stream] vs @racket[unsafe-in-stream] on fully memoized stream.})
    (define w (for/stream/values ([i (in-range 100000)]) (values i (add1 i))))
    (code:comment @#,t{Fully memoize @racket[w] first.})
    (for ([(a b) (in-stream w)]) (void))
    (time (for ([(a b) (in-stream w)]) (void)))
    (time (for ([(a b) (unsafe-in-stream w)]) (void)))
  ]
}
