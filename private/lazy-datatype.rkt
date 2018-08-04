#lang lazy
(require (only-in racket/base [begin r:begin])
         (for-syntax scheme/list
                     syntax/parse
                     (only-in scheme/function curry)
                     racket/base))

(provide define-type type-case)

(define-for-syntax (plai-syntax-error id stx-loc format-string . args)
  (raise-syntax-error 
   id (apply format (cons format-string args)) stx-loc))

(define-for-syntax (map/syntax f . stxes)
  (apply map f (map syntax->list stxes)))

(define bug:fallthru-no-else
  (string-append 
   "You have encountered a bug in the PLAI code.  (Error: type-case "
   "fallthru on cond without an else clause.)"))
(define-for-syntax bound-id
  (string-append
   "identifier is already bound in this scope (If you didn't define it, "
   "it was defined by the PLAI language.)"))
(define-for-syntax type-case:generic
  (string-append
   "syntax error in type-case; search the Help Desk for `type-case' for "
   "assistance."))
(define-for-syntax define-type:duplicate-variant
  "this identifier has already been used")
(define-for-syntax type-case:not-a-type
  "this must be a type defined with define-type")
(define-for-syntax type-case:not-a-variant
  "this is not a variant of the specified type")
(define-for-syntax type-case:argument-count
  "this variant has ~a fields, but you provided bindings for ~a fields")
(define-for-syntax type-case:missing-variant
  "syntax error; probable cause: you did not include a case for the ~a variant, or no else-branch was present")
(define-for-syntax type-case:unreachable-else
  "the else branch of this type-case is unreachable; you have matched all variants")
(define-for-syntax define-type:zero-variants
  "you must specify a sequence of variants after the type, ~a")

(define-for-syntax ((assert-unbound stx-symbol) id-stx)
  (when (identifier-binding id-stx)
    (plai-syntax-error stx-symbol id-stx bound-id)))

(define-for-syntax (assert-unique variant-stx)
  (let ([dup-id (check-duplicate-identifier (syntax->list variant-stx))])
    (when dup-id
      (plai-syntax-error 'define-type dup-id 
                         define-type:duplicate-variant))))

(define-for-syntax type-symbol (gensym))

(define-for-syntax (validate-and-remove-type-symbol stx-loc lst)
  (if (and (list? lst) (eq? type-symbol (first lst)))
      (rest lst)
      (plai-syntax-error 'type-case stx-loc type-case:not-a-type)))

(define-for-syntax (syntax-string s)
  (symbol->string (syntax-e s)))

(define-syntax (define-type stx)
  (define (format-id/std fmt x)
    (datum->syntax x
                   (string->symbol
                    (format fmt (syntax-e x)))
                   x
                   x))
  (syntax-parse
   stx
   [(_ datatype:id
       [variant:id (field:id field/c:expr) ...]
       ...)
    
    ; Ensure we have at least one variant.
    (when (empty? (syntax->list #'(variant ...)))
      (plai-syntax-error 'define-type stx define-type:zero-variants
                         (syntax-e #'datatype)))
    
    ; Ensure variant names are unique.
    (assert-unique #'(variant ...))
    ; Ensure each set of fields have unique names.
    (map/syntax assert-unique #'((field ...) ...))
    
    ; Ensure type and variant names are unbound
    (map (assert-unbound 'define-type)
         (cons #'datatype? (syntax->list #'(variant ...))))
    (with-syntax
        ([(variant* ...) 
          (map (lambda (s)
                 (datum->syntax stx (syntax->datum s)))
               (generate-temporaries #'(variant ...)))])
      
      (with-syntax
          ([((field/c-val ...) ...)
            (map/syntax generate-temporaries #'((field/c ...) ...))]
           [datatype?
            (format-id/std "~a?" #'datatype)]
           [(variant? ...)
            (map/syntax (curry format-id/std "~a?") #'(variant ...))]
           [(variant*? ...)
            (map/syntax (curry format-id/std "~a?") #'(variant* ...))]
           [(make-variant ...)
            (map/syntax (curry format-id/std "make-~a") #'(variant ...))]
           [(make-variant* ...)
            (map/syntax (curry format-id/std "make-~a") #'(variant* ...))]
           [(struct:variant* ...)
            (map/syntax (curry format-id/std "struct:~a") #'(variant* ...))]
           [(variant*-ref ...)
            (map/syntax (curry format-id/std "~a-ref") #'(variant* ...))]
           [(variant*-set! ...)
            (map/syntax (curry format-id/std "~a-set!") #'(variant* ...))])
        
        (with-syntax
            ([((f:variant? ...) ...)
              (map/syntax (lambda (v? fs)
                            (map/syntax (lambda (f) v?) fs))
                          #'(variant? ...)
                          #'((field ...) ...))]
             [((variant-field ...) ...)
              (map/syntax (lambda (variant fields)
                            (map/syntax (curry format-id/std (string-append (syntax-string variant) "-~a"))
                                        fields))
                          #'(variant ...)
                          #'((field ...) ...))]
             [((variant*-field ...) ...)
              (map/syntax (lambda (variant fields)
                            (map/syntax (curry format-id/std (string-append (syntax-string variant) "-~a"))
                                        fields))
                          #'(variant* ...)
                          #'((field ...) ...))]
             
             [((set-variant-field! ...) ...)
              (map/syntax (lambda (variant fields)
                            (map/syntax (curry format-id/std (string-append "set-" (syntax-string variant) "-~a!"))
                                        fields))
                          #'(variant ...)
                          #'((field ...) ...))]
             [((set-variant*-field! ...) ...)
              (map/syntax (lambda (variant fields)
                            (map/syntax (curry format-id/std (string-append "set-" (syntax-string variant) "-~a!"))
                                        fields))
                          #'(variant* ...)
                          #'((field ...) ...))]
             [((variant-field-pos ...) ...)
              (map/syntax (lambda (fields)
                            (for/list ([f (syntax->list fields)]
                                       [i (in-naturals)])
                              i))
                          #'((field ...) ...))]
             [(num-variant-fields ...)
              (map/syntax (lambda (fields)
                            (length (syntax->list fields)))
                          #'((field ...) ...))])
          
          (syntax/loc stx
            (r:begin
              (define-syntax datatype
                (list type-symbol
                      (list (list #'variant (list #'variant-field ...) #'variant?)
                            ...)
                      #'datatype?))
              ;; Use `make-struct-type' instead of `define-struct' just so we can set
              ;; the variant's constructor name (without colliding with the `variant'
              ;; constructor binding further below):
              (define-values (struct:variant*
                              make-variant* 
                              variant*?
                              variant*-ref
                              variant*-set!)
                (make-struct-type 'variant 
                                  #f  ; no supertype
                                  num-variant-fields
                                  0 #f ; no auto fields
                                  null 
                                  #f ; inspector => transparent
                                  #f ; not a procedure
                                  null)) ; all fields mutable
              ...
              (define-values (variant*-field ...)
                (let ([ref variant*-ref])
                  (values
                   (make-struct-field-accessor 
                    ref
                    variant-field-pos
                    'field:id)
                   ...)))
              ...
              (define-values (set-variant*-field! ...)
                (let ([set variant*-set!])
                  (values
                   (make-struct-field-mutator
                    set
                    variant-field-pos
                    'field:id)
                   ...)))
              ...
              (define (variant? x) (variant*? (! x)))
              ...
              (define (datatype? x)
                (or (variant? x) ...))
              (r:begin
                (define make-variant
                  (lambda (field ...)
                    (let ([name 'make-variant])
                      ;; No contract checking, which forces too much
                      ;(unless (field/c field) (raise-type-error name (format "<~s>" 'field/c) field))
                      ;...
                      (void))
                    (make-variant* field ...)))
                (define variant
                  (lambda (field ...)
                    (let ([name 'variant])
                      ;(unless (field/c field) (raise-type-error name (format "<~s>" 'field/c) field))
                      ;...
                      (void))
                    (make-variant* field ...)))
                (define variant-field
                  (lambda (v)
                    (variant*-field (! v))))
                ...
                (define set-variant-field!
                  (lambda (v nv)
                    (set-variant*-field! (! v) nv)))
                ...
                )
              ...)))))]))

;;; Asserts that variant-id-stx is a variant of the type described by 
;;; type-stx.
(define-for-syntax ((assert-variant type-info) variant-id-stx)
  (unless (ormap (λ (stx) (free-identifier=? variant-id-stx stx)) 
                 (map first type-info))
    (plai-syntax-error 'type-case variant-id-stx type-case:not-a-variant)))

;;; Asserts that the number of fields is appropriate.
(define-for-syntax ((assert-field-count type-info) variant-id-stx field-stx)
  (let ([field-count
         (ormap (λ (type) ; assert-variant first and this ormap will not fail
                  (and (free-identifier=? (first type) variant-id-stx)
                       (length (second type))))
                type-info)])
    (unless (= field-count (length (syntax->list field-stx)))
      (plai-syntax-error 'type-case variant-id-stx type-case:argument-count 
                         field-count (length (syntax->list field-stx))))))

(define-for-syntax ((ensure-variant-present stx-loc variants) variant)
  (unless (ormap (λ (id-stx) (free-identifier=? variant id-stx))
                 (syntax->list variants))
    (plai-syntax-error 'type-case stx-loc type-case:missing-variant 
                       (syntax->datum variant))))

(define-for-syntax ((variant-missing? stx-loc variants) variant)
  (not (ormap (λ (id-stx) (free-identifier=? variant id-stx))
              (syntax->list variants))))


(define-syntax (lookup-variant stx)
  (syntax-case stx ()
    [(_ variant-id ((id (field ...) id?) . rest))
     (free-identifier=? #'variant-id #'id)
     #'(list (list field ...) id?)]
    [(_ variant-id (__ . rest)) #'(lookup-variant variant-id rest)]
    [(_ variant-id ()) (error 'lookup-variant "variant ~a not found (bug in PLAI code)"
                              (syntax-e #'variant-id))]))

(define-for-syntax (validate-clause clause-stx)
  (syntax-case clause-stx ()
    [(variant (field ...) body ...)
     (cond
       [(not (identifier? #'variant))
        (plai-syntax-error 'type-case #'variant
                           "this must be the name of a variant")]
       [(ormap (λ (stx) 
                 (and (not (identifier? stx)) stx)) (syntax->list #'(field ...)))
        => (λ (malformed-field)
             (plai-syntax-error
              'type-case malformed-field
              "this must be an identifier that names the value of a field"))]
       [(not (= (length (syntax->list #'(body ...))) 1))
        (plai-syntax-error 
         'type-case clause-stx
         (string-append
          "there must be just one body expression in a clause, but you "
          "provided ~a body expressions.")
         (length (syntax->list #'(body ...))))]
       [else #t])]
    [(variant (field ...))
     (plai-syntax-error
      'type-case clause-stx
      "this case is missing a body expression")]
    [_ 
     (plai-syntax-error
      'type-case clause-stx
      "this case is missing a field list (possibly an empty field list)")]))

(define-syntax (bind-fields-in stx)
  (syntax-case stx ()
    [(_ (binding-name ...) case-variant-id ((variant-id (selector-id ...) ___) . rest) value-id body-expr)
     (if (free-identifier=? #'case-variant-id #'variant-id)
         #'(let ([binding-name (selector-id value-id)]
                 ...)
             body-expr)
         #'(bind-fields-in (binding-name ...) case-variant-id rest value-id body-expr))]))

(define-syntax (type-case stx)
  (syntax-case stx (else)
    [(_ type-id test-expr [variant (field ...) case-expr] ... [else else-expr])
     ; Ensure that everything that should be an identifier is an identifier.
     (and (identifier? #'type-id)
          (andmap identifier? (syntax->list #'(variant ...)))
          (andmap (λ (stx) (andmap identifier? (syntax->list stx)))
                  (syntax->list #'((field ...) ...))))
     (let* ([info (validate-and-remove-type-symbol
                   #'type-id (syntax-local-value #'type-id (λ () #f)))]
            [type-info (first info)]
            [type? (second info)])
       
       ; Ensure all names are unique
       (assert-unique #'(variant ...))
       (map assert-unique (syntax->list #'((field ...) ...)))
       
       ; Ensure variants are valid.
       (map (assert-variant type-info) (syntax->list #'(variant ...)))
       
       ; Ensure field counts match.
       (map (assert-field-count type-info) 
            (syntax->list #'(variant ...))
            (syntax->list #'((field ...) ...)))
       
       ; Ensure some variant is missing.
       (unless (ormap (variant-missing? stx #'(variant ...)) 
                      (map first type-info))
         (plai-syntax-error 'type-case stx type-case:unreachable-else))
       
       
       #`(let ([expr test-expr])
           (if (not (#,type? expr))
               #,(syntax/loc #'test-expr 
                   (error 'type-case "expected a value from type ~a, got: ~a"
                          'type-id
                          expr))
               (cond
                 [(let ([variant-info (lookup-variant variant #,type-info)])
                    ((second variant-info) expr))
                  (bind-fields-in (field ...) variant #,type-info expr case-expr)]
                 ...
                 [else else-expr]))))]
    [(_ type-id test-expr [variant (field ...) case-expr] ...)
     ; Ensure that everything that should be an identifier is an identifier.
     (and (identifier? #'type-id)
          (andmap identifier? (syntax->list #'(variant ...)))
          (andmap (λ (stx) (andmap identifier? (syntax->list stx)))
                  (syntax->list #'((field ...) ...))))
     (let* ([info (validate-and-remove-type-symbol 
                   #'type-id (syntax-local-value #'type-id (λ () #f)))]
            [type-info (first info)]
            [type? (second info)])
       
       ; Ensure all names are unique
       (assert-unique #'(variant ...))
       (map assert-unique (syntax->list #'((field ...) ...)))
       
       ; Ensure variants are valid.
       (map (assert-variant type-info) (syntax->list #'(variant ...)))
       
       ; Ensure field counts match.
       (map (assert-field-count type-info) 
            (syntax->list #'(variant ...))
            (syntax->list #'((field ...) ...)))
       
       ; Ensure all variants are covered
       (map (ensure-variant-present stx #'(variant ...))
            (map first type-info))
       
       #`(let ([expr test-expr])
           (if (not (#,type? expr))
               #,(syntax/loc #'test-expr 
                   (error 'type-case "expected a value from type ~a, got: ~a"
                          'type-id
                          expr))
               (cond
                 [(let ([variant-info (lookup-variant variant #,type-info)])
                    ((second variant-info) expr))
                  (bind-fields-in (field ...) variant #,type-info expr case-expr)]
                 ...
                 [else (error 'type-case bug:fallthru-no-else)]))))]
    ;;; The remaining clauses are for error reporting only.  If we got this
    ;;; far, either the clauses are malformed or the error is completely
    ;;; unintelligible.
    [(_ type-id test-expr clauses ...)
     (map validate-clause (syntax->list #'(clauses ...)))]
    [_ (plai-syntax-error 'type-case stx type-case:generic)]))
