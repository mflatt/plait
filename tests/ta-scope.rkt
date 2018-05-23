#lang plait

(define-type-alias (F 'a 'b) ('a -> 'b))

(define (a [x : 'a] [g : (F Number 'a)])
  (list x (g 10)))

(test (a "hello" (lambda (x) "there"))
      (list "hello" "there"))
