#lang racket/base
(require racket/vector
         (prefix-in lazy: lazy)
         "tuple.rkt")

(provide !!)

(define (!! v)
  ;; Force more values, including transparent structs.
  ;; Mutable values --- boxes and vectors --- are mutated
  ;; to replace original values with forced values.
  (define saw (make-hasheq))
  (let loop ([v v])
    (let ([v (lazy:! v)])
      (cond
        [(hash-ref saw v #f) v]
        [(pair? v)
         (let ([a (loop (car v))]
               [d (loop (cdr v))])
           (if (and (eq? a (car v))
                    (eq? d (cdr v)))
               v
               (cons a d)))]
        [(box? v)
         (hash-set! saw v v)
         (set-box! v (loop (unbox v)))
         v]
        [(vector? v)
         (cond
           [(immutable? v)
            (define new-v
              (for/vector #:length (vector-length v) ([i (in-range (vector-length v))])
                (loop (vector-ref v i))))
            (if (for/and ([i (in-range (vector-length v))])
                  (eq? (vector-ref v i) (vector-ref new-v i)))
                v
                new-v)]
           [else
            (hash-set! saw v v)
            (for ([i (in-range (vector-length v))])
              (vector-set! v i (loop (vector-ref v i))))
            v])]
        [(tuple? v)
         (define c (loop (tuple-content v)))
         (if (eq? v (tuple-content v))
             v
             (tuple c))]
        [(struct? v)
         (define-values (st skipped?) (struct-info v))
         (cond
           [(and st (not skipped?))
            (define-values (name count auto-count
                                 access mutate
                                 immutable super
                                 skipped?)
              (struct-type-info st))
            (cond
              [(and (not super)
                    (not skipped?)
                    (zero? auto-count))
               (define vec
                 (for/vector #:length count ([i (in-range count)])
                   (access v i)))
               (define vec2 (loop (vector-copy vec)))
               (cond
                 [(for/and ([e (in-vector vec)]
                            [e2 (in-vector vec2)])
                    (eq? e e2))
                  v]
                 [else
                  (apply (struct-type-make-constructor st) (vector->list vec2))])]
              [else v])]
           [else v])]
        [else v]))))
