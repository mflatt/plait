#lang plait

(define (s-exp-match? pattern s)
  (local [;; main matching routine is called after checking
          ;; that the pattern is well-formed:
          (define (s-exp-match? pattern s)
            (cond
             [(s-exp-list? pattern)
              (and (s-exp-list? s)
                   (list-match? (s-exp->list pattern)
                                (s-exp->list s)))]
             [(eq? 'ANY (s-exp->symbol pattern))
              #t]
             [(eq? 'SYMBOL (s-exp->symbol pattern))
              (s-exp-symbol? s)]
             [(eq? 'NUMBER (s-exp->symbol pattern))
              (s-exp-number? s)]
             [(eq? 'STRING (s-exp->symbol pattern))
              (s-exp-string? s)]
             [else
              ;; Any other symbol is a literal:
              (and (s-exp-symbol? s)
                   (eq? (s-exp->symbol s)
                        (s-exp->symbol pattern)))]))
          
          ;; check a list of s-expr against a list of patterns,
          ;; handling '... among the patterns
          (define (list-match? patterns ses)
            (cond
             [(empty? patterns)
              (empty? ses)]
             [(and (cons? (rest patterns))
                   (s-exp-symbol? (first (rest patterns)))
                   (eq? '... (s-exp->symbol (first (rest patterns)))))
              ;; handle ...
              (cond
               [(= (- (length patterns) 2)
                   (length ses))
                ;; 0 matches may work
                (list-match? (rest (rest patterns)) ses)]
               [(empty? ses)
                #f]
               [else
                ;; Need at least 1 match, then try again:
                (and (s-exp-match? (first patterns)
                                   (first ses))
                     (list-match? patterns
                                  (rest ses)))])]
             [(empty? ses)
              #f]
             [else
              (and (s-exp-match? (first patterns)
                                 (first ses))
                   (list-match? (rest patterns)
                                (rest ses)))]))

          ;; check that `pattern' is well-formed:
          (define (check-pattern pattern)
            (cond
             [(s-exp-list? pattern)
              (check-patterns (s-exp->list pattern) #f)]
             [(not (s-exp-symbol? pattern))
              (error 's-exp-match? 
                     (string-append "bad pattern: "
                                    (to-string pattern)))]
             [(eq? '... (s-exp->symbol pattern))
              ;; if `check-patterns' didn't see it, it's misplaced
              (error 's-exp-shape? "misplaced `...' in pattern")]
             [else 
              ;; any other symbol is ok --- either special or literal
              (void)]))

          ;; check that a list of patterns is ok, possibly with
          ;; `...', but only one `...':
          (define (check-patterns patterns saw-dots?)
            (cond
             [(empty? patterns) (void)]
             [(and (cons? (rest patterns))
                   (s-exp-symbol? (first (rest patterns)))
                   (eq? '... (s-exp->symbol (first (rest patterns)))))
              (if saw-dots?
                  (error 's-exp-shape? "multiple `...' in pattern")
                  (check-patterns (rest (rest patterns)) #t))]
             [else
              (begin
                (check-pattern (first patterns))
                (check-patterns (rest patterns) saw-dots?))]))]
    (begin
      (check-pattern pattern)
      (s-exp-match? pattern s))))

(module+ test
  (print-only-errors #t)
  (test (s-exp-match? `NUMBER `10)
        #t)
  (test (s-exp-match? `NUMBER `a)
        #f)
  (test (s-exp-match? `SYMBOL `a)
        #t)
  (test (s-exp-match? `SYMBOL `"a")
        #f)
  (test (s-exp-match? `STRING `"a")
        #t)
  (test (s-exp-match? `STRING `("a"))
        #f)
  (test (s-exp-match? `ANY `("a"))
        #t)
  (test (s-exp-match? `ANY `10)
        #t)
  (test (s-exp-match? `any `10)
        #f)
  (test (s-exp-match? `any `any)
        #t)

  (test (s-exp-match? `(SYMBOL) `(a))
        #t)
  (test (s-exp-match? `(SYMBOL) `(a b))
        #f)
  (test (s-exp-match? `(SYMBOL SYMBOL) `(a b))
        #t)
  (test (s-exp-match? `((SYMBOL) SYMBOL) `((a) b))
        #t)
  (test (s-exp-match? `((SYMBOL) NUMBER) `((a) b))
        #f)
  (test (s-exp-match? `((SYMBOL) NUMBER ((STRING))) `((a) 5 (("c"))))
        #t)
  (test (s-exp-match? `(lambda (SYMBOL) ANY) `(lambda (x) x))
        #t)
  (test (s-exp-match? `(lambda (SYMBOL) ANY) `(function (x) x))
        #f)

  (test (s-exp-match? `(SYMBOL ...) `(a b))
        #t)
  (test (s-exp-match? `(a ...) `(a b))
        #f)
  (test (s-exp-match? `(a ...) `(a a))
        #t)
  (test (s-exp-match? `(a ...) `())
        #t)
  (test (s-exp-match? `(a ... b) `())
        #f)
  (test (s-exp-match? `(a ... b) `(b))
        #t)
  (test (s-exp-match? `(a ... b) `(a a a b))
        #t)
  (test (s-exp-match? `((a ...) b ...) `((a a a) b b b b))
        #t)
  (test (s-exp-match? `((a ...) b ...) `((a a a) b c b b))
        #f)

  (test/exn (s-exp-match? `... `10)
            "misplaced")
  (test/exn (s-exp-match? `(...) `10)
            "misplaced")
  (test/exn (s-exp-match? `(a ... b ...) `10)
            "multiple")
  (test/exn (s-exp-match? `10 `10)
            "bad pattern")

  )
