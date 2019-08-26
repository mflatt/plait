#lang plait

(require (rename-in
          (typed-in racket/base
                    [expt : (Number Number -> Number)]
                    [+ : (Number Number -> Number)]
                    [vector-immutable : ('a 'a 'a -> (Vectorof 'a))])
          [+ plus]))

(define x : S-Exp `(a 2 "c" '(d) #f))
(define (f [y : Number]) y)

(print-only-errors #t)

(test 1024 (expt 2 10))
(test 7 (plus 3 4))
(test (vector 3 2 0) (vector-immutable 3 2 0))
(test (make-vector 3 2) (vector-immutable 2 2 2))
(test (make-vector 3 "apple") (vector-immutable "apple" "apple" "apple"))

(define apply-identity : (('a -> 'a) 'a -> (Listof 'a))
  (lambda (id x)
    (list (id x) (id x))))

(module+ test
  (test (box 10) (box-number 10)))
(module+ other
  (+ (unbox (box 10)) (unbox (box-number 10))))

(box-number : (Number -> (Boxof Number)))
(define box-number
  (lambda (n)
    (if (= n 0)
        (box 0)
        (box n))))

(test 12 (+ (max (add1 7) 0) (min (sub1 5) 9)))

(test #t (zero? (- (ceiling (floor (remainder 5 3)))
                   (identity (modulo 5 3)))))
(test #t (odd? 7))
(test #f (even? 7))

(test 3 (length (list "a" "b" "c")))
(test "b" (list-ref (list "a" "b" "c") 1))
(test "b" (second (list "a" "b" "c")))
(test "c" (third (list "a" "b" "c")))
(test "d" (fourth (list "a" "b" "c" "d")))

(test #t (s-exp-list? x))
(test #f (s-exp-string? x))
(test #f (s-exp-symbol? x))
(test #f (s-exp-number? x))
(test #f (s-exp-boolean? x))
(test (symbol->s-exp 'a) (first (s-exp->list x)))
(test #t (s-exp-symbol? (first (s-exp->list x))))
(test #t (s-exp-number? (first (rest (s-exp->list x)))))
(test #t (s-exp-string? (first (rest (rest (s-exp->list x))))))
(test #t (s-exp-list? (first (rest (rest (rest (s-exp->list x)))))))
(test #t (s-exp-boolean? (first (rest (rest (rest (rest (s-exp->list x))))))))
(test #t (s-exp-string? (string->s-exp "a")))
(test #t (s-exp-number? (number->s-exp 2)))
(test #t (s-exp-boolean? (boolean->s-exp #f)))
(test #f (s-exp->boolean `#f))
(test #t (s-exp-list? (list->s-exp (list))))
(test #t (s-exp-list? (list->s-exp (list (number->s-exp 2)))))

(test 5 (local [(x : Number)
                (define x 10)]
          (begin
            (set! x 5)
            x)))

(test 5 (call/cc (lambda (x) 5)))
(test 5 (let/cc x 5))
(test 7 (local [(define y (lambda (q) (+ q 3)))
                (y : (Number -> Number))]
          (if (= 0 (call/cc (lambda (k)
                              (begin
                                (set! y k)
                                0))))
              (y 2)
              7)))
(test 7 (local [(define y (lambda (q) (+ q 3)))]
          (if (= 0 (let/cc k
                     (begin
                       (set! y k)
                       0)))
              (y 2)
              7)))

(test 15 (local [(x : 'a)
                 (y : 'a)
                 (define x 10)
                 (define y "apple")]
           (+ x (string-length y))))

(test 15 (local [(ident : ('a -> 'a))
                 (define (ident x)
                   x)]
           (+ (ident 10) (string-length (ident "hello")))))

(local [(define x (lambda ((x : String)) x))]
  (set! x (lambda (y) (string-append y y))))

(define il (list (lambda (x) x)))
(test 5 ((first il) 5))
(test "a" ((first il) "a"))

(define-type (T 'a) 
  (v [f : ('a -> 'a)])
  (ordinal [n : Number]))
(define i (v (lambda (x) x)))
(test 10 ((v-f i) 10))
(test "a" ((v-f i) "a"))
(test #t (v? i))
(test #f (v? (ordinal 8)))

(define-type-alias IntT (T Number))
(define-type-alias (XT 'x 'y) (T 'x))
(test 7 ((lambda ([i : IntT]) (type-case (T Number) i
                                [(v f) (f 6)]
                                [(ordinal n) n]))
         (v (lambda (x) (+ 1 x)))))
(test 7 ((lambda ([i : (XT Number String)]) (type-case (T Number) i
                                              [(v f) (f 6)]
                                              [(ordinal n) n]))
         (v (lambda (x) (+ 1 x)))))

(test 5 (type-case (T Number) i
          [(ordinal n) n]
          [else 5]))

(test #t (letrec ([even? (lambda (n)
                           (if (= 0 n)
                               #t
                               (odd? (- n 1))))]
                  [odd? (lambda (n)
                          (if (= 0 n)
                              #f
                              (even? (- n 1))))])
           (even? 10)))
(test (list 3 1 2) (let ([x 1]
                         [y 2]
                         [z 3])
                     (let ([x z]
                           [y x]
                           [z y])
                       (list x y z))))
(test 4 (let* ([x 1]
               [y 2]
               [x y]
               [y x])
          (+ y y)))

(test (list 2 4 6) (filter (lambda (x) (not (= x 5)))
                           (list 2 5 5 4 6 5)))
(test 10 (foldl (lambda (x n) (+ x n))
                0
                (list 1 2 3 4)))
(test "1234" (foldr (lambda (x n) (string-append (to-string x) n))
                    ""
                    (list 1 2 3 4)))

(test 2 (case 'apple
          [(donut) 1]
          [(apple banana) 2]
          [else 5]))

(test 5 (case 'apple
          [else 5]))

(test 2 (case 3
          [(0) 1]
          [(2 3) 2]
          [else 5]))

(test 7 (cond
         [(= 0 1) 6]
         [else 7]))

(test/exn (cond) "no matching")
(test/exn (cond [#f 10]) "no matching")
(test/exn (case 'apple) "no matching")
(test/exn (case 'apple [(banana) 12]) "no matching")

(define vd : Void (void))

(define-type SharedGraph$
  (node [s : String]
        [next : (Listof SharedGraph$)]))

(define g1 : (Listof SharedGraph$)
  (shared ([PVD (node "Providence" (list ORH BOS))]
           [ORH (node "Worcester" (list PVD BOS))]
           [BOS (node "Boston" (list PVD ORH))])
    (list PVD ORH BOS)))

(define n (if #f
              (+ (time 10) 1)
              0))

(test 5 (length (build-list 5 (lambda (i) (if (zero? i) "s" "f")))))

(when (zero? 5) 1 "x")
(unless (odd? 5) 1 "x")

(test #t (member 1 (list 3 2 1)))
(test #f (member 6 (list 3 2 1)))

(test 1 (type-case (Optionof 'a) (none)
          [(none) 1]
          [(some v) 2]))
(test 5 (type-case (Optionof 'a) (some 5)
          [(none) 1]
          [(some v) v]))

(test #t (some? (some 5)))
(test #t (none? (none)))
(test #f (none? (some 5)))
(test #f (some? (none)))

(define sid (some (lambda (x) x)))
(test 5 ((some-v sid) 5))
(test "5" ((some-v sid) "5"))

(test '(2 3) (type-case (Listof Number) '(1 2 3)
               [(cons a b) b]
               [empty empty]))
(test 1 (type-case (Listof Number) '(1 2 3)
          [(cons a b) a]
          [empty 5]))
(test 5 (type-case (Listof Number) '()
          [(cons a b) a]
          [empty 5]))

(define (my-length l)
  (type-case (Listof 'a) l
    [empty 0]
    [(cons a b) (+ 1 (my-length b))]))
(test 5 (my-length '(1 2 3 4 5)))
(test 4 (my-length '(a b c d)))

(define ht (make-hash (list)))
(test (none) (hash-ref ht "a"))
(test (void) (hash-set! ht "a" 1))
(test (some 1) (hash-ref ht "a"))
(test (list "a") (hash-keys ht))
(test (void) (hash-remove! ht "a"))
(define ht2 (make-hash (list (values 1 'a) (values 3 'b))))
(test (some 'a) (hash-ref ht2 1))
(test (some 'b) (hash-ref ht2 3))
(test (none) (hash-ref ht2 5))
(test 4 (let ([l (hash-keys ht2)])
          (+ (first l) (second l))))

(define ht3 (hash (list (values "a" 'a) (values "b" 'b))))
(test (some 'a) (hash-ref ht3 "a"))
(test (some 'b) (hash-ref ht3 "b"))
(define ht4 (hash-set ht3 "c" 'c))
(test (some 'a) (hash-ref ht4 "a"))
(test (some 'c) (hash-ref ht4 "c"))
(define ht5 (hash-remove ht4 "b"))
(test (some 'a) (hash-ref ht5 "a"))
(test (some 'c) (hash-ref ht5 "c"))
(test (none) (hash-ref ht5 "b"))

(define-type Linked-List
  (llnode [s : String]
          [next : (Optionof Linked-List)]))

(define lls (shared ([x (llnode "a" (some x))]
                     [y (some (llnode "b" (none)))])
              (list x (some-v y))))
(test "a" (llnode-s (some-v (llnode-next (first lls)))))
(test "b" (llnode-s (second lls)))

(test #t (s-exp-symbol? `a))
(test `3 (second (s-exp->list `(a ,(number->s-exp (+ 1 2)) c))))
(test `4 (third (s-exp->list `(a ,@(list `3 `4) c))))
(test (second (s-exp->list `(a `,@(list `3 `4) c))) 
      `(quasiquote (unquote-splicing (list `3 `4))))

(define kons cons)
(define boxed-null (box (list)))

(module+ test
  (define (kons v) v)
  (define apple-string "apple")
  (define-type Fish (trout [n : Number]) (bass))
  (test apple-string (llnode-s (llnode (kons apple-string)
                                       (kons (none))))))

(module+ test
  (test "apple" apple-string)
  (define (fry f)
    (type-case Fish f
      [(trout n) (+ n 1)]
      [(bass) 0])))

(include "for-basic.rktl")
(test "hello6" (string-append included-string
                              (to-string (+ included-num 1))))

(define-syntax twice
  (syntax-rules ()
    [(twice a)
     (+ a a)]
    [(twice a b)
     (+ (+ a a) (+ b b))]))
(test 10 (twice 5))
(test 16.2 (twice 6.0 2.1))

(define-syntax-rule (define-seven a b)
  (splice
   (define a 7)
   (define b 7)))
(define-seven seven-a seven-b)
(test 7 seven-a)
(test 7 seven-b)

(define-syntax (a-macro stx)
  #'(+ 1 2))
(test 3 (a-macro whatever you like))

(test 'ok (begin
            (local ((define vertical (lambda (tree) : (Optionof 'a)
                                             (vertical tree))))
              vertical)
            'ok))

(define prm (make-parameter 5))
(define sprm (make-parameter "5"))
(test 5 (parameter-ref prm))
(test (void) (parameter-set! prm 7))
(test 7 (parameter-ref prm))
(test 8
      (parameterize ([prm 8])
        (parameter-ref prm)))
(define (get-prm) (parameter-ref prm))
(define get-prm-getter : ((Parameterof 'a) -> (-> 'a))
  (lambda (p)
    (lambda ()
      (parameter-ref p))))
(test 7 ((get-prm-getter prm)))
(test "5" ((get-prm-getter sprm)))

(define (add-char s c) (string-append s (list->string (list c))))
(test #\a (string-ref "cat" 1))
(test "at" (substring "catch" 1 3))
(test 3 (string-length "cat"))
(test (list #\c #\a #\t) (string->list "cat"))
(test "cat" (list->string (list #\c #\a #\t)))
(test "cat!" (add-char "cat" #\!))

(test (list "cat") (let ([g (lambda (x) (list x))])
                     (let ([f g]) ; identifiers are values
                       (if #f
                           (if (zero? (first (f 10))) (list "a") (list "b"))
                           (f "cat")))))

(require (opaque-type-in racket/base
                         [Bstring bytes?])
         (typed-in racket/base 
                   [bytes : (Number Number -> Bstring)]
                   [bytes-ref : (Bstring Number -> Number)]))
(test 10 (bytes-ref (bytes 5 10) 1))
(define (extract-first s)
  (bytes-ref s 0))
(define (generate-bstring a b) : Bstring
  (bytes a b))

(define flonum? #t)
(require (rename-in (opaque-type-in racket/base
                                    [fl flonum?])
                    [fl Flonum]
                    [flonum? real-flonum?])
         (typed-in racket/base
                   [exact->inexact : (Number -> Flonum)])
         (typed-in racket/flonum
                   [fl+ : (Flonum Flonum -> Flonum)]))
(test (exact->inexact 10.0) (fl+ (exact->inexact 6) (exact->inexact 4)))

(define (poly g)
  ;; check that a non-polymorphic binding inside
  ;; doesn't break polymorphism of the enclosing
  ;; function
  (let ([x (g)])
    x))

(test 1 (poly (lambda () 1)))
(test "x" (poly (lambda () "x")))


;; Check expansion of a type abbreviation into
;; a type abbreviation:
(define-type-alias (MapFunc 'a 'b) ('a -> 'b))
(define-type-alias (Env 'v) (MapFunc Symbol 'v))
(define (lookup [var : Symbol]
                [env : (Env 'v)])
  (error 'NotImpl "NotImplemented"))

(test 5 (fst (pair 5 #t)))
(test #t (snd (pair 5 #t)))

(define (add-two l)
  (+ (first (has-type l : (Listof Number)))
     (has-type (second l) : Number)))
(test 3 (add-two (list 1 2)))

(define (build-hash classhash)
  (hash-set classhash `(something) 'another))
(test (some 'another) (hash-ref (build-hash (hash (list))) `(something)))

;; does define-values work correctly?
(define-values (aoeua aoeub) (values 3 "banana"))
(test 3 aoeua)
(test "banana" aoeub)
(define-values ([aoeuc : Number] [aoeud : String]) (values 4 "plum"))
(test 4 aoeuc)
(test "plum" aoeud)

(test (cond
        [(< 2 1) ....]
        [else 'ok])
      'ok)
(test (cond
        [(< 2 1) (.... 'oops)]
        [else 'ok])
      'ok)

(define tuple-test (values 1 2))
