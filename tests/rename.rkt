#lang plait

(require (rename-in
          [typed-in racket (map : (('a 'b -> 'c)
                                   (Listof 'a)
                                   (Listof 'b) -> (Listof 'c)))]
          [map rkt-map2]))
(require (rename-in
          [typed-in racket (map : (('a 'b 'c -> 'd)
                                   (Listof 'a)
                                   (Listof 'b)
                                   (Listof 'c) -> (Listof 'd)))]
          [map rkt-map3]))

(print-only-errors #t)
(test (list 4 6) (rkt-map2 + (list 1 2) (list 3 4)))
(test (list 9 12) (rkt-map3 (lambda (x y z) (+ (+ x y) z)) (list 1 2) (list 3 4) (list 5 6)))
