#lang scribble/manual
@require[scribble/example
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

@title{stream-values}
@author+email["Sorawee Porncharoenwase" "sorawee.pwase@gmail.com"]

@defmodule[stream-values]

This library allows manipulation of multiple values in
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{streams}.

@deftogether[(@defform[(stream-cons/values first-expr rest-expr)]
              @defform[(stream/values expr ...)]
              @defform[(stream*/values expr ...)]
              @defform[(for/stream/values (for-clause ...) body-or-break ... body)]
              @defform[(for*/stream/values (for-clause ...) body-or-break ... body)])]{
  Like @racket[stream-cons], @racket[stream], @racket[stream*], @racket[for/stream],
  and @racket[for*/stream], but they support
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{multiple values}.

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
  Similar to @racket[in-stream] (which supports
  @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{multiple values}
  already), but it cooperates with this library so that an
  @racket[unsafe-in-stream] application can provide better performance for iteration
  on streams (that are constructed via this library) when @racket[unsafe-in-stream]
  appears directly in a @racket[for] clause. It is @emph{unsafe} in a sense that
  stream memoization (which is a feature of Racket streams) is @emph{not guaranteed}.
  That is, for each element in the stream, an iteration via @racket[unsafe-in-stream]
  might or might not memoize the element.
  Moreover, if a stream is previously memoized, @racket[unsafe-in-stream] does
  @emph{not guarantee} that it will use the memoized result.
  @;{However, it does guarantee that if a stream
  is @emph{fully} memoized, iterating on the stream will use the memoized result.}

  This procedure is useful when a stream is used in the iteration only once and then
  discarded, since memoization does not matter and the iteration could be
  significantly faster. On the other hand,
  if the stream will be used again in the future, the lack of memoization could
  result in a performance loss or even a surprisingly incorrect result.

  @for-element-reachability["stream"]
  @examples[#:eval sequence-evaluator
    (code:comment @#,elem{Performance gain compare to @racket[in-stream]})
    (define s (for/stream/values ([i (in-range 10000000)]) (values i (add1 i))))
    (time (for ([(a b) (in-stream s)]) (void)))
    (time (for ([(a b) (unsafe-in-stream s)]) (void)))
    code:blank
    code:blank
    (code:comment @#,elem{Lack of memoization})
    (define xs (for/stream/values ([i (in-range 5)]) (displayln i)))
    (for ([_ (in-stream xs)]) (void))
    (code:comment @#,elem{This iteration should not display any element because the stream is memoized.})
    (for ([_ (in-stream xs)]) (void))
    (define ys (for/stream/values ([i (in-range 5)]) (displayln i)))
    (for ([_ (unsafe-in-stream ys)]) (void))
    (code:comment @#,elem{Due to the lack of memoization, this iteration displays elements again.})
    (for ([_ (unsafe-in-stream ys)]) (void))
  ]
}

@;{
code:blank
code:blank
(code:comment @#,elem{Fully memoization stream is utilized})
(define zs (for/stream/values ([i (in-range 5)]) (displayln i)))
(for ([_ (in-stream zs)]) (void))
(code:comment @#,elem{This iteration should not display any element because the stream is fully memoized.})
(for ([_ (unsafe-in-stream zs)]) (void))
}