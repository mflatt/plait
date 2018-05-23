#lang racket
(require rackunit)

(define (syn-test exp reg)
  (define ns (make-base-namespace))
  (check-regexp-match
   (if (string? reg) (regexp-quote reg) reg)
   (with-handlers ((exn:fail:syntax? exn-message))
     (parameterize ([current-namespace ns])
       (eval exp)
       "NO SYNTAX ERROR"))))

(syn-test
 '(module m plait
    
    (define-type TEnv
      (mt)
      (bind [x : symbol]
            [t : string]
            [rest : TEnv]))
    
    (define (lookup a-tenv id)
      (type-case TEnv a-tenv
        [mt (error 'lookup "ack!")]
        [(bind x t rest)
         (if (equal? x id)
             t
             (lookup rest))])))
 #rx"type-case: .*expected a parenthesized variant name.*mt.*")

;; Double-check value restrction:
(syn-test
 '(module m plait
    (local [(define f (local [(define b (box (list)))]
                        (lambda (x sel?)
                          (if sel?
                              (first (unbox b))
                              (begin
                                (set-box! b (list x))
                                x)))))]
      (begin
        (f 10 #f)
        (string-append "x" (f "hi" #t)))))
 #rx"typecheck failed: Number vs. String")
          
;; Check that polymorphism inference in nested scopes
;; doesn't go wrong:
(syn-test
 '(module m plait
    
    (define member : ('a 'b -> 'c)
      (lambda (e l)
        #f))
    
    (local [(define (in? n)
              (member n n))]
      (if (string=? "in?" "in?") 
          in?
          (lambda (n) (void)))))
 #rx"typecheck failed: Void vs. Boolean")

(syn-test
 '(module m plait
    (quote #"x"))
 #rx"not an identifier")

(syn-test
 '(module m plait
    (quasiquote #"x"))
 #rx"disallowed content")

(syn-test
 '(module m plait
    (quasiquote unquote))
 #rx"bad syntax")

(syn-test
 '(module m plait
    (quasiquote (unquote 5)))
 #rx"Number vs. S-Expression")

(syn-test
 '(module m plait
    (quasiquote (1 (unquote-splicing 5) 3)))
 #rx"Number vs. .Listof S-Expression.")


(syn-test
 '(module m plait
    (define b (let ([y (box (list))])
                (lambda () y)))
    (define c b)
    (set-box! (c) (list 1))
    (string-append (first (unbox (c))) "x"))
 #rx"String vs. Number|Number vs. String")

(syn-test
 '(module m plait
    (define a (box (lambda (x) x)))
    (define (set v)
      (set-box! a v))
    (set (lambda (x) (+ x 1)))
    (set (lambda (x) (string-append x "1"))))
 #rx"String vs. Number|Number vs. String")

(syn-test
 '(module m plait
    (define x "x")
    (module+ test (+ 1 x)))
 #rx"String vs. Number|Number vs. String")

(syn-test
 '(module m plait
    (case 1
      [(1) 5]
      [(a) 6]))
 #rx"number")

(syn-test
 '(module m plait
    (case 1
      [(a) 5]
      [(1) 6]))
 #rx"number")

(syn-test
 '(module m plait
    (case 1
      [(a) 5]
      [(b) 6]))
 #rx"Number vs. Symbol|Symbol vs. Number")

(syn-test
 '(module m plait
    (case 1
      [else 6]))
 #rx"Number vs. Symbol|Symbol vs. Number")

(syn-test
 '(module m plait
    (has-type 1 : Symbol))
 #rx"Number vs. Symbol|Symbol vs. Number")

(syn-test
 '(module m plait
    (define (->) 3))
 #rx"cannot redefine a keyword")

(syn-test
 '(module m plait
    (define-values (z ->) 3))
 #rx"cannot redefine a keyword")

(syn-test
 '(module m plait
    (define-values (z [-> : Number]) 3))
 #rx"cannot redefine a keyword")

(syn-test
 '(module m plait
    (2))
 #rx"call of a non-function")

(syn-test
 '(module m plait
    (empty))
 #rx"call of a non-function")

(syn-test
 '(module m plait
    (define (f x) x)
   (f))
 #rx"wrong number of arguments")

(syn-test
 '(module m plait
   (define (f x) (x 1))
   (f 10))
 #rx"typecheck failed: [^\n]*vs.")

(syn-test
 '(module m plait
   (define (f x) (x 1))
   (f (lambda () 10)))
 #rx"typecheck failed: [^\n]*vs.")
