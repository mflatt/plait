#lang racket/base

(define ns (make-base-empty-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'plait))

(eval '(require "macro-intro.rkt") ns)

(unless (equal? (eval '(#%top-interaction . (sum 3 4)) ns) 7)
  (error "wrong sum"))

(unless (equal? (eval '(#%top-interaction . (join "a" "b")) ns) "ab")
  (error "wrong join"))

(unless (with-handlers ([exn:fail? (lambda (exn) (regexp-match? "typecheck failed"
                                                                (exn-message exn)))])
          (eval '(#%top-interaction . (sum 3 "a")) ns)
          #f)
  (error "should have failed"))

;; untyped import, but can't access macro:

(define u-ns (make-base-namespace))
(eval '(require "macro-intro.rkt") u-ns)

(eval '(module use plait
         (require "macro-intro.rkt")
         (values (sum 1 2) (join "a" "b")))
      u-ns)
(eval '(require 'use) u-ns)
