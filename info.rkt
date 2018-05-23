#lang setup/infotab

(define collection "plait")

(define deps '("base"
               "plai"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     ["scribble-lib" #:version "1.16"]))

(define version "0.1")

(define scribblings '(("plait.scrbl" (multi-page) (language))))
