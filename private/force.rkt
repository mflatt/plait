#lang racket/base
(require (prefix-in lazy: lazy)
         "tuple.rkt")

(provide !!)

(define (!! v)
  ;; Force more values, including transparent structs
  (let loop ([v v])
    (let ([v (lazy:! v)])
      (cond
        [(pair? v)
         (let ([a (loop (car v))]
               [d (loop (cdr v))])
           (if (and (eq? a (car v))
                    (eq? d (cdr v)))
               v
               (cons a d)))]
        [(box? v)
         (let ([c (loop (unbox v))])
           (if (eq? c (unbox v))
               v
               (box c)))]
        [(vector? v)
         (define v2
           (for/vector #:length (vector-length v) ([e (in-vector v)])
             (loop e)))
         (if (for/and ([e (in-vector v)]
                       [e2 (in-vector v2)])
               (eq? e e2))
             v
             v2)]
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
               (define vec2 (loop vec))
               (cond
                 [(eq? vec vec2) v]
                 [else
                  (apply (struct-type-make-constructor st) (vector->list vec2))])]
              [else v])]
           [else v])]
        [else v]))))

             
             
               
       
       
