#lang plait

(define-syntax (define-1st-order-function stx)
  (syntax-case stx ()
    [(_ (rator rand ...) body)
     #'(splice
        (define (F rand ...) body)
        (define-syntax rator
          (syntax-rules ()
            [(_ rand ...)
             (F rand ...)])))]))

(define-1st-order-function (sum n1 n2)
  (+ n1 n2))

(define-1st-order-function (join n1 n2)
  (string-append n1 n2))

(sum 1 2)
(join "a" "b")
