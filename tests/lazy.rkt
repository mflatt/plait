#lang plait #:lazy

(print-only-errors #t)

(define (f x) 1)

(define (h x) (f x))

(test 1 (h (/ 1 0)))

(define (g y) (+ y 1))

(test 11 (g 10))

(test
 (letrec ([x (cons 1 x)])
   (first (map add1 (filter odd? (rest (rest x))))))
 2)

(test
 (let ([b (box (/ 1 0))])
   'ok)
 'ok)

(test
 (let ([b (vector 10 (/ 1 0))])
   (vector-ref b 0))
 10)

(test (fst (values 17 (/ 1 0)))
      17)

(test (snd (values (/ 1 0) 18))
      18)

(test (snd (pair (/ 1 0) 18))
      18)

;; ----------------------------------------

(test (let ([a 5] [f (lambda (x y) y)])
        (f
          (set! a 6)
          a))
      5)

(test (let ([a 5])
        (let ([b (set! a 6)])
          (if (equal? (void) b) a -1)))
      6)

;; ----------------------------------------

(define-type (MyList 'a)
  (my-empty)
  (my-cons [a : 'a]
           [d : (MyList 'a)]))

(define my-first my-cons-a)
(define my-rest my-cons-d)

(test (letrec ([x (my-cons 1 x)])
        (my-first (my-rest (my-rest x))))
      1)

;; ----------------------------------------

(test (type-case (Optionof String) (some "Hello")
        [(some s) s]
        [(none) "Bye"])
      "Hello")
(test (type-case (Optionof String) (none)
        [(some s) s]
        [(none) "Bye"])
      "Bye")
(test (type-case (Optionof Number) (some (/ 1 0))
        [(some s) "Whatever"]
        [(none) "Bye"])
      "Whatever")

;; ----------------------------------------

(test (and (zero? 1) (zero? 0))
      #f)
(test (or (zero? 1) (zero? 0))
     #t)

(test (case (+ 1 2)
        [(3) 'ok]
        [else 'no])
      'ok)

;; ----------------------------------------

(define (show-the-result)
  'this-result-should-print)

(show-the-result)
