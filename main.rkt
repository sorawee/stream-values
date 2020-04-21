#lang racket

(provide stream-cons/values for/stream/values for*/stream/values
         stream/values stream*/values unsafe-in-stream)
(require syntax/parse/define
         (only-in racket/private/stream-cons stream-lazy)
         (only-in racket/private/for split-for-body)
         (prefix-in r: racket))

(module+ test
  (require rackunit))

;; ----------------------------------------------------------------------------

;; fst-evaled is either #f or a list of values
;; rst-evaled is either #f or a stream
(struct stream (fst-thunk fst-evaled rst-thunk rst-evaled)
  #:mutable
  #:methods gen:stream
  [(define (stream-empty? self) #f)
   (define (stream-first self)
     (cond
       [(stream-fst-evaled self) => (curry apply values)]
       [else
        (define xs (call-with-values (stream-fst-thunk self) list))
        (set-stream-fst-evaled! self xs)
        (apply values xs)]))
   (define (stream-rest self)
     (cond
       [(stream-rst-evaled self)]
       [else (define s (stream-lazy ((stream-rst-thunk self))))
             (set-stream-rst-evaled! self s)
             s]))])

(define-syntax-rule (stream-cons/values fst rst)
  (stream (thunk fst) #f (thunk rst) #f))

(module+ test
  (test-case "stream-cons: basic retrieval"
    (check-equal? (call-with-values (thunk (r:stream-first (r:stream-cons
                                                            (values 1)
                                                            r:empty-stream)))
                                    list)
                  (list 1))
    (check-equal? (call-with-values (thunk (r:stream-first (stream-cons/values
                                                            (values 1 2)
                                                            r:empty-stream)))
                                    list)
                  (list 1 2)))

  (test-case "stream-cons: retrieval"
    (check-equal? (call-with-values (thunk (r:stream-ref (r:stream-cons
                                                          (values 'a)
                                                          (r:stream-cons
                                                           (values 'b)
                                                           r:empty-stream))
                                                         1))
                                    list)
                  (list 'b))
    (check-equal? (call-with-values (thunk (r:stream-ref (stream-cons/values
                                                            (values 'a 'b)
                                                            (stream-cons/values
                                                             (values 'c 'd)
                                                             r:empty-stream))
                                                         1))
                                    list)
                  (list 'c 'd)))

  (test-case "stream-cons: memoization"
    (define x 0)
    (define s (r:stream-cons (set! x (add1 x)) empty-stream))
    (r:stream-first s)
    (r:stream-first s)
    (check-equal? x 1)

    (define y 0)
    (define t (stream-cons/values (set! y (add1 y)) empty-stream))
    (r:stream-first t)
    (r:stream-first t)
    (check-equal? y 1))

  (test-case "stream-cons: laziness"
    (define (loop) (loop))
    (check-true (r:stream? (r:stream-rest (r:stream-cons 0 (loop)))))
    (check-true (r:stream? (r:stream-rest (stream-cons/values 0 (loop)))))

    (check-true (r:stream? (r:stream-cons (loop) r:empty-stream)))
    (check-true (r:stream? (stream-cons/values (loop) r:empty-stream))))

  (test-case "stream-cons: error"
    (check-exn #px"rest expression produced a non-stream"
               (thunk (r:stream-first (r:stream-rest (r:stream-cons 0 1)))))
    (check-exn #px"rest expression produced a non-stream"
               (thunk (r:stream-first (r:stream-rest (stream-cons/values 0 1))))))

  (test-case "stream-cons: for"
    (check-equal? (for/sum ([(a) (r:stream-cons
                                  (values 1)
                                  (r:stream-cons
                                   (values 2)
                                   r:empty-stream))])
                    a)
                  3)
    (check-equal? (for/sum ([(a b) (stream-cons/values
                                    (values 1 2)
                                    (stream-cons/values
                                     (values 3 4)
                                     r:empty-stream))])
                    (+ a b))
                  10)))

;; ----------------------------------------------------------------------------

(define (assert-stream? who st)
  (if (r:stream? st)
      st
      (raise-argument-error who "stream?" st)))

;; ----------------------------------------------------------------------------

(define-syntaxes (for/stream/values for*/stream/values)
  (let ()
    (define ((make-for/stream derived-stx) stx)
      (syntax-case stx ()
        [(_ clauses . body)
         (with-syntax ([((pre-body ...) (post-body ...))
                        (split-for-body stx #'body)])
           (quasisyntax/loc stx
             (#,derived-stx #,stx
                            ([get-rest r:empty-stream]
                             #:delay-with thunk)
               clauses
               pre-body ...
               (stream-cons/values (let () post-body ...) (get-rest)))))]))
    (values (make-for/stream #'for/foldr/derived)
            (make-for/stream #'for*/foldr/derived))))

(module+ test
  (test-case "for/stream: basic"
    (check-equal? (sequence->list (in-values-sequence (for/stream ([x 5]) x)))
                  '((0) (1) (2) (3) (4)))
    (check-equal? (sequence->list (in-values-sequence (for/stream/values ([x 5])
                                                        (values x (add1 x)))))
                  '((0 1) (1 2) (2 3) (3 4) (4 5))))

  (test-case "for/stream: laziness"
    (check-true (r:stream? (for/stream ([x (in-naturals)]) x)))
    (check-true (r:stream? (for/stream/values ([x (in-naturals)]) x)))

    (define x 0)
    (define s (for/stream ([_ 5]) (set! x (add1 x))))
    (check-equal? x 0)
    (r:stream-first s)
    (check-equal? x 1)
    (r:stream-first s)
    (check-equal? x 1)
    (r:stream-rest s)
    (check-equal? x 1)
    (r:stream-first (r:stream-rest s))
    (check-equal? x 2)
    (r:stream-first (r:stream-rest s))
    (check-equal? x 2)

    (define y 0)
    (define t (for/stream/values ([_ 5]) (set! y (add1 y))))
    (check-equal? y 0)
    (r:stream-first t)
    (check-equal? y 1)
    (r:stream-first t)
    (check-equal? y 1)
    (r:stream-rest t)
    (check-equal? y 1)
    (r:stream-first (r:stream-rest t))
    (check-equal? y 2)
    (r:stream-first (r:stream-rest t))
    (check-equal? y 2)))

(define-syntax stream/values
  (syntax-rules ()
    ((_) r:empty-stream)
    ((_ hd tl ...) (stream-cons/values hd (stream/values tl ...)))))

(define-syntax stream*/values
  (syntax-rules ()
    [(_ tl) (assert-stream? 'stream*/values tl)]
    [(_ hd tl ...) (stream-cons/values hd (stream*/values tl ...))]))

(module+ test
  (test-case "stream"
    (check-equal? (sequence->list (in-values-sequence (r:stream 1 2 3)))
                  '((1) (2) (3)))
    (check-equal? (sequence->list (in-values-sequence (stream/values (values 1 2)
                                                                     (values 2 3)
                                                                     (values 3 4))))
                  '((1 2) (2 3) (3 4))))

  (test-case "stream*"
    (check-equal? (for/list ([x (r:stream* -2 -1 (in-naturals))] [_ 5]) x)
                  '(-2 -1 0 1 2))
    (check-equal? (for/list ([(a b)
                              (stream*/values (values -2 -1)
                                              (values -1 0)
                                              (for/stream/values ([x (in-naturals)])
                                                (values x (add1 x))))]
                             [_ 5])
                    (list a b))
                  '((-2 -1) (-1 0) (0 1) (1 2) (2 3)))


    (check-equal? (for/list ([a (stream* -1 (in-naturals))] [_ 5]) a)
                  '(-1 0 1 2 3))
    (check-equal? (for/list ([a (stream*/values -1 (in-naturals))] [_ 5]) a)
                  '(-1 0 1 2 3))))

;; ----------------------------------------------------------------------------

(define (stream-not-empty? s)
  (cond
    [(r:stream? s) (not (r:stream-empty? s))]
    [else (raise-argument-error 'unsafe-in-stream "stream?" s)]))

(define-sequence-syntax unsafe-in-stream
  (λ () #'values)
  (syntax-parser
    [[(iter ...) (_ the-stream)]
     #'[(iter ...)
        (:do-in
         ()
         (void)
         ([current the-stream])
         (stream-not-empty? current)
         ([(iter ...) (if (stream? current)
                          ((stream-fst-thunk current))
                          #;(cond
                            [(stream-fst-evaled current) => (curry apply values)]
                            [else ((stream-fst-thunk current))])
                          (r:stream-first current))]
          [(rst) (if (stream? current)
                     ((stream-rst-thunk current))
                     #;(or (stream-rst-evaled current) ((stream-rst-thunk current)))
                     (r:stream-rest current))])
         #t
         #t
         (rst))]]))

(module+ test
  (test-case "unsafe-in-stream/values: basic"
    (check-equal? (sequence->list (in-values-sequence (unsafe-in-stream
                                                       (stream/values (values 1 2)
                                                                      (values 3 4)))))
                  '((1 2) (3 4))))

  (test-case "unsafe-in-stream: with memoize"
    (define x 0)
    (define s (r:stream* (set! x (add1 x)) (stream/values (set! x (add1 x)))))
    (for ([_ (in-stream s)]) (void))
    (check-equal? x 2)
    (for ([_ (in-stream s)]) (void))
    (check-equal? x 2)

    (define y 0)
    (define t (r:stream* (set! y (add1 y)) (stream/values (set! y (add1 y)))))
    (for ([_ (unsafe-in-stream t)]) (void))
    (check-equal? y 2)
    (for ([_ (unsafe-in-stream t)]) (void))
    (check-equal? y 2))

  (test-case "unsafe-in-stream: with no memoize"
    (define x 0)
    (define s (stream*/values (set! x (add1 x)) (r:stream (set! x (add1 x)))))
    (for ([_ (in-stream s)]) (void))
    (check-equal? x 2)
    (for ([_ (in-stream s)]) (void))
    (check-equal? x 2)

    (define y 0)
    (define t (stream*/values (set! y (add1 y)) (r:stream (set! y (add1 y)))))
    (for ([_ (unsafe-in-stream t)]) (void))
    (check-equal? y 2)
    (for ([_ (unsafe-in-stream t)]) (void))
    (check-equal? y 4))

  #;(test-case "unsafe-in-stream: guarantee read memoize"
    (define y 0)
    (define t (stream*/values (set! y (add1 y)) (r:stream (set! y (add1 y)))))
    (for ([_ (in-stream t)]) (void))
    (check-equal? y 2)
    (for ([_ (unsafe-in-stream t)]) (void))
    (check-equal? y 2)))
