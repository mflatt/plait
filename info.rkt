#lang setup/infotab

(define collection "plait")

(define deps '("base"
               "lazy"
               "plai"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     ["scribble-lib" #:version "1.16"]))

(define test-omit-paths '("scribblings/demo.rkt"))

(define version "0.1")

(define scribblings '(("scribblings/plait.scrbl" (multi-page) (language))))
