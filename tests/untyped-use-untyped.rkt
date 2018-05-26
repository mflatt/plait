#lang plait/untyped
(require "untyped.rkt")

(define-syntax-rule (test a b)
  (unless (equal? a b)
    (error 'test "failed: ~.s" `b)))

(test `(6 6) (apply-identity (lambda (x) 6) 5))
