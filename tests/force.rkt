#lang racket/base
(require plait/private/force)

;; Example from David Bremner:
(module rabbit plait
  (define-type Hat
    [empty-hat]
    [hat (bunny : Rabbit)])

  (define-type Rabbit
    [rabbit  [hat : (Boxof Hat)]])

  (define (looper)
    (let*
        ([the-box (box (empty-hat))]
         [the-rabbit (rabbit the-box)]
         [the-hat (hat the-rabbit)])
      (begin
        (set-box! the-box the-hat)
        the-hat))))
(require 'rabbit)

(define thumper (looper))

(unless (equal? (format "~v" thumper)
                (format "~v" (!! thumper)))
  (error 'force "didn't work right on cyclic value"))

;; ----------------------------------------

(module sloth plait #:lazy
  (define-type Sloth
    [sloth (size : Number)])
  (define speedy (sloth (+ 1 2))))
(require 'sloth)

(unless (equal? "(sloth 3)" (format "~v" (!! speedy)))
  (error 'force "force didn't force"))
