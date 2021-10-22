#lang scribble/manual
@(require (for-label (only-meta-in 0 plait))
          (for-syntax racket/base)
          scribble/racket
          scribble/example)

@(define-syntax-rule (define-r r:lambda r:syntax-rules)
  (begin
   (require (for-label racket/base))
   (define-syntax r:lambda
     (make-element-id-transformer
      (lambda (stx) #'@racket[lambda])))
   (define-syntax r:syntax-rules
     (make-element-id-transformer
      (lambda (stx) #'@racket[syntax-rules])))))
@(define-r r:lambda r:syntax-rules)

@(define demo (make-base-eval #:lang 'plait))
@(define demo2 (make-base-eval #:lang 'plait))

@(begin
  (define-syntax-rule (define-racket-shared racket-shared)
    (begin
     (require (for-label (only-in racket/shared shared)))
     (define racket-shared @racket[shared])))
  (define-racket-shared racket-shared))

@(define (tutorial tag)
   @margin-note{For an introduction, see the tutorial section @secref[tag].})

@title{Plait Language}

@defmodulelang[plait]

The Plait language syntactically resembles the
@racketmodname[plai] language, which is based on
@racketmodname[racket], but the type system is close to that of
@hyperlink["http://smlnj.org/"]{ML}.

@table-of-contents[]

@; --------------------------------------------------

@include-section["tutorial.scrbl"]

@; --------------------------------------------------

@section[#:tag "Definitions"]{Definitions}

The body of a @schememodname[plait] module is a sequence of
definitions, expressions and type declarations. The module implicitly exports all
top-level definitions. When a @racketmodname[plait] module is
imported into a module that does not use @racketmodname[plait],
the imports have contracts (matching reflecting the exported bindings'
types).

@defform[#:link-target? #f
         #:id [: #':]
         #:literals (:)
         (id : type)]{

Declares that @racket[id] has type @racket[type]. This type
declaration and the definition of @racket[id] must be within the same
definition sequence, either at the top of a module or together in 
a set of @racket[local] definitions.

@history[#:added "1.1"]}

@defform*/subs[#:literals (:)
               [(define id expr)
                (define id : type expr)
                (define (id id/type ...) expr)
                (define (id id/type ...) : type expr)]
               ([id/type id
                         [id : type]])]{

@tutorial["definitions-tutorial"]

Defines @racket[id].

The @racket[expr] in each of the first two forms is evaluated to get
the value of @racket[id]. In the first form, the type of @racket[id]
is inferred at the type of @racket[expr], while the second form
declares a specific type for @racket[id].

The third and fourth forms define @racket[id] as a function, where
each @racket[id/type] is a function argument (with an optional declare
type) and @racket[expr] is the body of the function, which is
evaluated when the function is called. In the fourth form, a
@racket[type] before the body @racket[expr] declares the function's
result type (which must match the type of @racket[expr]).

Note that the first and second forms of @racket[define] also define
functions in the case that @racket[expr] produces a function, such as
when @racket[expr] is a @racket[lambda] form. The third and fourth
forms are simplify shorthands for defining a function.

Evaluating a reference to @racket[id] before its definition is
evaluated triggers an ``undefined identifier'' error.

@examples[#:eval demo
(define a 1)
a
(define b : Number (+ 1 2))
b
(define (c x)
  (+ x b))
(c 3)
(define (d [y : Number]) : Number
  (c y))
(d 4)
]}


@defform/subs[#:literals (:)
              (define-values (id/type ...) expr)
              ([id/type id
                        [id : type]])]{

@tutorial["tuples-tutorial"]

Defines each @racket[id/type] (with an optional type declaration) to
be the values within the @tech{tuple} produced by @racket[expr], which
must have as many values as declared @racket[id/type]s.

@examples[#:eval demo2
(define t (values 1 'one "One"))
(define-values (a b c) t)
a
(define-values ([x : Number] [b : Symbol] [c : String]) t)
c]}


@defform/subs[#:literals (: quote)
              (define-type tyid/abs
                (variant-id [field-id : type])
                ...)
              ([tyid/abs id
                         (id '@#,racket[_arg-id] ...)])]{

@tutorial["datatypes-tutorial"]

Defines a type (when @racket[tyid/abs] is @racket[id]) or type
constructor (when @racket[tyid/abs] has the form @racket[(id 'id
...)]).

A constructor @racket[variant-id] is defined for each variant. Each
constructor takes an argument for each field of its variant, where the
type of each field is declared by the @racket[type] after each
@racket[field-id]. The result type of each constructor is
@racket[id].

Instances of a type declared with @racket[define-type] are normally
used through @racket[type-case].

In addition to the type and constructors, a @racket[define-type]
expression also defines:

@itemize[

  @item{for each variant, a predicate
    @racketidfont{@racket[variant-id]?} that returns @racket[#t] when
    applied to an instance of @racket[variant-id] and @racket[#f] for
    any other value; and}

  @item{for each field of each variant, an accessor
    @racketidfont{@racket[variant-id]-@racket[field-id]} that takes a
    instance of @racket[variant-id] and returns the value of the field
    corresponding to @racket[field-id].}
]

@examples[#:eval demo
(define-type Shape
  (circle [radius : Number])
  (rectangle [width : Number]
             [height : Number]))
(define cr (circle 10))
cr
(circle? cr)
(circle-radius cr)
(define rc (rectangle 2 3))
(+ (rectangle-width rc) (rectangle-height rc))
]}

@defform/subs[#:literals (quote)
              (define-type-alias tyid/abs type)
              ([tyid/abs id
                         (id '@#,racket[_arg-id] ...)])]{

Defines a type alias @racket[id]. When @racket[tyid/abs] is
@racket[id], then using @racket[id] is the same as using
@racket[type]. When @racket[tyid/abs] is @racket[(id '@#,racket[_arg-id] ...)], then
using @racket[(id _arg-type ...)] is the same as using @racket[type]
with each @racket['@#,racket[_arg-id]] replaced by the corresponding @racket[_arg-type].

@examples[#:eval demo
(define-type-alias Size Number)
(define (square-area [side : Size])
  (* side side))
(square-area 10)
]

Except for @racket[_arg-id]s, the @racket[type] form must not
reference any type variables that do not yet have a scope.}


@defform/subs[#:literals (typed-in opaque-type-in rename-in :)
              (require spec ...)
              ([spec module-path
                     (typed-in module-path [id : type] ...)
                     (opaque-type-in module-path [type-id predicate-id] ...)
                     (rename-in spec [orig-id new-id] ...)])]{

@tutorial["program-tutorial"]

Imports from each @racket[module-path].

When a @racket[module-path] is not wrapped with @racket[typed-in] or @racket[opaque-type-in], then
@racket[module-path] must refer to a module that is implemented with
@racketmodname[plait].

When @racket[module-path] is wrapped with @racket[typed-in], then only the
specified @racket[id]s are imported from @racket[module-path], and the
type system assumes (without static or additional dynamic checks) the
given @racket[type] for each @racket[id].

When @racket[module-path] is wrapped with @racket[opaque-type-in],
then the corresponding @racket[type-id]s are bound as opaque
datatypes, where @racket[predicate-id] from @racket[module-path] is a
run-time predicate (used for contracts as needed for cooperation with
untyped code) for instances of the datatype.

@examples[#:eval demo
(require (typed-in racket/base [gensym : (-> Symbol)]))
(gensym)]}


@defform[(trace id ...)]{

@tutorial["testing-tutorial"]

Traces subsequent calls---showing arguments and results---for
functions bound to the @racket[id]s.  This form can be used only in a
module top level, and only for tracing functions defined within the
module.}


@defform[(module id module-path form ...)]{

Declares a submodule named @racket[id], which can be required in the
enclosing module using @racket['@#,racket[id]] or @racket[(submod "."
id)]:

@racketblock[
 (module sub plait
   (define n 8))
 (require 'sub)
 (+ n 1)
]}


@defform[(module+ id form ...)]{

@tutorial["program-tutorial"]

Declares/extends a submodule named @racket[id], which is particularly
useful for defining a @racketidfont{test} submodule to hold tests that
precede relevant definitions (since the submodule implicitly imports
the bindings of its enclosing module, and DrRacket or @exec{raco test}
runs the @racketidfont{test} submodule):

@racketblock[
 (module+ test
   (test 11 (add-one 10)))

 (define (add-one n)
   (+ 1 n))
]}


@defform[(include path-spec)]{

Copy the content of @racket[path-spec] in place of the @racket[include]
form, which can only be used in a top-level position.}

@deftogether[(
@defform[(define-syntax-rule (id pattern ...) template)]
@defform*/subs[#:literals (r:syntax-rules r:lambda)
               [(define-syntax id macro-expr)
                (define-syntax (id arg-id) macro-body ...)]
               ([macro (r:syntax-rules ....)
                       (r:lambda ....)])]
)]{
Defines a macro. In a @racket[macro-expr] or @racket[macro-body], the bindings of
@racketmodname[racket/base] are available.

A macro of the form

@racketblock[
(define-syntax-rule (id pattern ...) template)
]

is equivalent to

@racketblock[
(define-syntax id
  (r:syntax-rules ()
   [(id pattern ...) template]))
]}


@defform[(splice form ...)]{

Equivalent to the @racket[form]s sequence in a module or top-level context,
which is useful for producing multiple definitions from a macro.}

@; ----------------------------------------

@section{Expressions}

An expression can be a literal constant that is a number (type
@racket[Number]), a string (type @racket[String]), a symbol (type
@racket[Symbol]) written with @racket[quote] or @litchar{'},
an @tech{S-expression} (type
@racket[S-Exp]) written with @racket[quasiquote] or @litchar{`},
@racket[#t] (type @racket[Boolean]), @racket[#f] (type
@racket[Boolean]), or a character (type @racket[Char]). 
An expression also can be a bound identifier (in
which case its type comes from its binding).

@defidform[....]{

The @racket[....] form is intended as a placeholder for a expression.
It can be used in any expression position and can have any type, but
@racket[....] reports an error when evaluated.

@examples[#:eval demo
(if #f .... 'ok)
(eval:error (if #t .... 'no))
]}

@defform[#:literals (:)
         (has-type expr : type)]{

Equivalent to @racket[expr], but declares/asserts that @racket[expr]
has type @racket[type].

@examples[#:eval demo
(has-type 1 : Number)
(eval:error (has-type "a" : Number))]}


@defform/subs[(quote q-form)
              ([q-form id
                       Number
                       String
                       Boolean
                       (q-form ...)
                       (code:line @#,tt{#}(q-form ...))
                       (code:line @#,tt{#&}q-form)])]{

@tutorial["lists-tutorial"]

The @racket[quote] form is usually written as just a @litchar{'}
before a @racket[q-form]; that is, @racket['@#,racket[id]] and
@racket[(@#,racket[quote] id)] are equivalent.

The @racket[quote] form produces a symbol, number, string, boolean,
list, vector, or box value:

@itemlist[

 @item{A @racket[quote]d @racket[id] produces a symbol. For example,
       @racket['hello] produces the symbol whose letters are
       @litchar{hello}.}

 @item{A @racket[quote]d @racket[Number], @racket[String], or
       @racket[Boolean] produces the @racket[Number], @racket[String],
       or @racket[Boolean] itself. For example, @racket['1] is the
       same as @racket[1].}

 @item{A @racket[quote]d parenthesized form produces a list containing
       the @racket[quote]d values within the parentheses. For example,
       @racket['(1 2 3)] produces a list containing @racket[1],
       @racket[2], and @racket[3]. Similarly, @racket['(a b c)]
       produces a list containing three symbols, since @racket['a],
       @racket['b], and @racket['c] are symbols.}

 @item{A @racket[quote]d @tt{#}@racket[(q-form ...)] produces a @tech{vector}. The
       vector is immutable, though, so @racket[vector-set!] will not
       work on the vector.}

 @item{A @racket[quote]d @tt{#&}@racket[q-form] produces a @tech{box}. The
       box is immutable, though, so @racket[set-box!] will not work on
       the box.}

]

Beyond the syntactic contraints of @racket[q-form], the resulting list
must have a type. So, for example, @racket[quote] cannot create a list
that mixes numbers and symbols.

@examples[#:eval demo
'a
'(1 2 3)
(eval:error '(1 a))
'((a) (b c))
]}

@deftogether[(
@defform/subs[#:literals (unquote unquote-splicing quasiquote)
              (quasiquote qq-form)
              ([qq-form id
                        Number
                        String
                        Boolean
                        (qq-form ...)
                        (#,(racket unquote) expr)
                        (#,(racket unquote-splicing) expr)
                        (#,(racket quasiquote) expr)])]
@defidform[unquote]
@defidform[unquote-splicing]
)]{

@tutorial["s-exp-tutorial"]

The @racket[quasiquote] form is similar to @racket[quote],
but it produces an @tech{S-expression},
and it supports escapes via @racket[unquote] and
@racket[unquote-splicing]. A @racket[(@#,racket[unquote] expr)] form
is replaced with the value of @racket[expr], while a
@racket[(@#,racket[unquote-splicing] expr)] form requires that
@racket[expr] produces a list and is replaced by the list content as
an inlined sequence of S-expressions.

The @racket[quasiquote] form is usually written as just a @litchar{`}
before @racket[qq-form]; that is, @racket['@#,racket[qq-form]] and
@racket[(@#,racket[quasiquote] qq-form)] are equivalent.

The @racket[unquote] form is usually written as just a @litchar{,}
before @racket[expr]; that is, @racket[, @#,racket[expr]] and
@racket[(@#,racket[unquote] expr)] are equivalent.

The @racket[unquote-splicing] form is usually written as just a
@litchar[",@"] before @racket[expr]; that is, @racket[,@@#,racket[expr]]
and @racket[(@#,racket[unquote-splicing] expr)] are equivalent.

With a nested @racket[quasiquote] form, escapes are preserved while
escaping to the enclosing level of quotation. For example,
@racket[``(,(+ 1 2))] is equivalent to @racket['(@#,racket[quasiquote] (@#,racket[unquote] 1))]
where the @racket[quasiquote] and @racket[unquote] symbols are preserved
in the result S-expression.

@examples[#:eval demo
`a
`(1 a)
`(+ ,(number->s-exp (+ 1 2)) 3)
`(+ ,@(list `1 `2) 3)
]}


@defform[(#%app expr expr ...)]{

A function call, which is normally written without the @racket[#%app]
keyword.

@examples[#:eval demo
(add1 1)]}


@defform*/subs[#:literals (:)
               [(lambda (id/ty ...) expr)
                (lambda (id/ty ...) : type expr)]
               ([id/ty id
                       [id : type]])]{

@tutorial["lambda-tutorial"]

An anonymous function which takes as many argument as specified
@racket[id/ty]s and produces the result of @racket[expr]. Each
argument has an optional type specification, and when a type is
written after @racket[(id/ty ...)], it declares the result type of the
function.

@examples[#:eval demo
(lambda (x) (+ x 1))
(lambda ([x : Number]) (+ x 1))
((lambda (x) (+ x 1)) 3)
(map (lambda (x) (+ x 1)) (list 1 2 3))]}


@defidform[λ]{An alias for @racket[lambda].}

@deftogether[(
@defform[(if test-expr expr expr)]
@defform*[#:literals (else)
          [(cond [test-expr expr] ...)
           (cond [test-expr expr] ... [else expr])]]
)]{

@tutorial["cond-tutorial"]

An @racket[if] form produces the value of the first @racket[expr] if
@racket[test-expr] produces true or the value of the second
@racket[expr] otherwise. Only one of the two @racket[expr]s is
evaluated.

A @racket[cond] form produces the value of the first @racket[expr]
whose @racket[test-expr] produces true. The @racket[test-expr]s are
tried in order until a true result is found, and at most one of the
@racket[expr]s is evaluated. If no @racket[test-expr] produces a true
result, a ``no matching clause`` exception is raised. An @racket[else]
in place of the last @racket[test-expr] is equivalent to
@racket[#t].

Each @racket[test-expr] must have type @racket[Boolean].

@examples[#:eval demo
(if (< 1 2)
    'less
    'greater-or-equal)
(cond
 [(< 2 1) 'bad]
 [(< 2 2) (begin (/ 1 0) 'oops)]
 [(< 2 3) 'ok]
 [(< 2 (/ 1 0)) 'oops]
 [else (begin (/ 1 0) 'oops)])]}


@defform*[#:literals (else)
          [(case val-expr [(id-or-number ...) expr] ...)
           (case val-expr [(id-or-number ...) expr] ... [else expr])]]{

Performs a case dispatch on a symbol or number. The value of the
@racket[case] form is the value of an @racket[expr] whose
@racket[(id-or-number ...)] sequence includes the result of
@racket[val-expr], when symbols are matched to identifiers. If no
@racket[id-or-number] matches, a ``no matching clause`` exception is
raised.

The dispatching mode, symbol or number, is inferred from the
@racket[id-or-number]s, which must all be symbols or numbers for a
given use of @racket[case]. If no clause provides a number or symbol,
then symbol dispatch is inferred.

@examples[#:eval demo
(case (+ 1 2)
  [(0 1 2) 'too-small]
  [(3) 'ok]
  [else 'other])
(case 'goodbye
  [(hello) 'hi]
  [(goodbye) 'bye])
]}
 

@defform[(begin expr ...+)]{

@tutorial["state-tutorial"]

Evaluates the @racket[expr]s in sequence, producing the result of the last @racket[expr].

@examples[#:eval demo
(+ (begin
    (display "hi\n")
    1)
   (begin
    (display "bye\n")
    2))
]}

@deftogether[(
@defform[(when test-expr expr ...+)]
@defform[(unless test-expr expr ...+)]
)]{

Conditionally evaluates @racket[expr]s for their side effects, always
returning @racket[(void)]. A @racket[when] form evaluates its
@racket[expr]s only @racket[test-expr] produces true, while an
@racket[unless] form evaluates its @racket[expr]s only
@racket[test-expr] produces false.

@examples[#:eval demo
(when (< 1 2) (display "yes"))
(unless (< 1 2) (display "no"))
]}


@deftogether[(
@defform[(local [definition-or-type-declaration ...] expr)]
@defform[(letrec ([id rhs-expr] ...) expr)]
@defform[(let ([id rhs-expr] ...) expr)]
@defform[(let* ([id rhs-expr] ...) expr)]
)]{

@tutorial["definitions-tutorial"]

Local binding forms. The @racket[local] form accommodates multiple
definitions and type declarations (using @racket[:])
that are visible only among the definitions and the body
@racket[expr]. The @racket[letrec], @racket[let], and @racket[let*]
forms bind each @racket[id] to the value of the corresponding
@racket[rhs-expr] (where the @racket[rhs-expr]s are evaluated in
order). In the case of @racket[letrec], each @racket[id] is visible to
every @racket[rhs-expr] as well as in the body @racket[expr]. In the
case of @racket[let], each @racket[id] is visible only in the body
@racket[expr]. In the case of @racket[let*], each @racket[id] is
visible only to later @racket[rhs-expr]s as well as in the body
@racket[expr].

@examples[#:eval demo
(local [(add-x : (Number -> Number))
        (x : Number)
        (define (add-x y) (+ x y))
        (define x 2)]
  (add-x 3))
(eval:error add-x)
(letrec ([add-x (lambda (y) (+ x y))]
         [x 2])
  (add-x 3))
(let ([x 1])
  (let ([add-x (lambda (y) (+ x y))]
        [x 2])
   (add-x 3)))
(let ([x 1])
  (let* ([add-x (lambda (y) (+ x y))]
         [x 2])
   (add-x 3)))   
(let ([x 1])
  (let* ([x 2]
         [add-x (lambda (y) (+ x y))])
   (add-x 3)))]}


@defform[(shared ([id expr] ...) expr)]{

Creates cyclic data for a restricted set of restricted @racket[expr]
patterns. See @racket-shared from @racketmodname[racket/shared] for a
description of allowed patterns, besides the additional restriction
that the form must be typed.

@examples[#:eval demo
(shared ([infinite-ones (cons 1 infinite-ones)])
  (list-ref infinite-ones 1001))
]}


@defform[(parameterize ([param-expr val-expr] ...) expr)]{

The @racket[parameterize] form implements a kind of dynamic binding.
Each @racket[param-expr] must have type @racket[(Parameterof _type)]
where the corresponding @racket[val-expr] has type @racket[_type], and
the parameter produces by @racket[param-expr] is set to
@racket[val-expr] for the dynamic extent of @racket[expr].

@examples[#:eval demo
(define current-mode (make-parameter 'straight))
(define (display-line)
  (display (case (parameter-ref current-mode)
            [(straight) "---"]
            [(curvy) "~~~"])))
(parameterize ([current-mode 'curvy])
  (display-line))
(display-line)
(define f
  (parameterize ([current-mode 'curvy])
    (lambda () (display-line))))
(f)
]}            


@defform[(set! id expr)]{

@tutorial["state-tutorial"]

Mutates @racket[id] to have the value of @racket[expr].

@examples[#:eval demo
(define x 1)
(set! x (+ 1 1))
x]}


@deftogether[(
@defform[(and expr ...)]
@defform[(or expr ...)]
)]{

@tutorial["cond-tutorial"]

Boolean combinations with short-circuiting: as soon as an
@racket[expr] produces false in @racket[and] or true in @racket[or],
the remaining @racket[expr]s are not evaluated. The value of
@racket[(and)] is true and @racket[(or)] is false. The @racket[expr]s
must have type @racket[Boolean].


@examples[#:eval demo
(and (< 1 2) (< 3 4))
(and (< 2 1) (< 3 (/ 1 0)))
(or (< 2 1) (< 3 4))
(or (< 2 1) (< 1 2) (< 3 (/ 1 0)))
]}


@defform[(list elem ...)]{

@tutorial["lists-tutorial"]

Builds a list. All @racket[elem]s must have the same type.

@examples[#:eval demo
(list 1 2 3)
(list "a" "b")
(list (list 1 2) empty (list 3 4))
]}


@defform[(vector elem ...)]{

@tutorial["state-tutorial"]

Builds a vector. All @racket[elem]s must have the same type.

@examples[#:eval demo
(vector 1 2 3)
(vector "a" "b")
(vector (list 1 2) empty (list 3 4))
]}


@defform[(values elem ...)]{

@tutorial["tuples-tutorial"]

Combines multiple values into @deftech{tuple}, where a tuple
containing one value is equivalent to the value. Match a @tech{tuple}
result using @racket[define-values].

The type of each @racket[elem] is independent.

@examples[#:eval demo
(values 1 'two "three")
]}


@defform*/subs[#:literals (quote else Listof empty cons)
               [(type-case tyid/abs val-expr
                  [(variant-id field-id ...) expr] ...)
                (type-case tyid/abs val-expr
                  [(variant-id field-id ...) expr] ...
                  [else expr])
                (type-case (Listof type) val-expr
                  [list-variant expr] ...)
                (type-case (Listof type) val-expr
                  [list-variant expr] ...
                  [else expr])]
               ([tyid/abs id
                          (id type ...)]
                [list-variant empty
                              (cons first-id rest-id)])]{

@tutorial["datatypes-tutorial"]

Dispatches based on the variant of the result of @racket[val-expr].

In the form that has @racket[tyid/abs], @racket[val-expr] must have type @racket[tyid/abs], and
@racket[tyid/abs] must refer to a type defined via
@racket[define-type]. The result is the value of @racket[expr] for the
@racket[variant-id] that is instantiated by @racket[val-expr] or the
@racket[expr] in an @racket[else] clause if no @racket[variant-id]
matches. Each @racket[field-id] is bound to a corresponding (by
position) value of a field within the variant instance for use in the
same clause's @racket[expr].

The number of @racket[field-id]s must match the number of fields
declared for @racket[variant-id] in the definition of
@racket[tyid/abs], and either every @racket[variant-id] of
@racket[tyid/abs] must have a clause in the @racket[type-case] form
or an @racket[else] clause must be present.

@examples[#:eval demo2
(define-type Shape
  (circle [radius : Number])
  (rectangle [width : Number]
             [height : Number]))
(define (area [s : Shape])
  (type-case Shape s
    [(circle r) (* (* r r) 3.14)]
    [(rectangle w h) (* w h)]))
(area (circle 1))
(area (rectangle 2 3))
]

In the @racket[(Listof type)] form, @racket[val-expr] must have type
@racket[(Listof type)]. Each non-@racket[else] clause is either a
@racket[(cons first-id rest-id)] clause or an @racket[empty] clause,
and both most appear without @racket[else], or at most one of those can
appear with @racket[else].

@examples[#:eval demo
(define (my-length l)
  (type-case (Listof 'a) l
    [empty 0]
    [(cons a b) (+ 1 (my-length b))]))
(length '(1 2 3))
(length '(a b))
]}

@defform[#:literals (lambda)
         (try expr (lambda () handle-expr))]{

Either returns @racket[expr]'s result or catches an exception raised
by @racket[expr] and calls @racket[handle-expr].

@examples[#:eval demo
(try 1 (lambda () 2))
(try (/ 1 0) (lambda () 2))
(try (begin (error 'demo "oops") 1) (lambda () 2))
(eval:error (try (begin (error 'demo "oops") 1) (lambda () (/ 2 0))))
]}


@deftogether[(
@defform[(test expr expr)]
@defform[(test/exn expr string-expr)]
)]{

@tutorial["testing-tutorial"]

The @racket[test] form checks whether the value of the first
@racket[expr] matches the value of the second @racket[expr], and
reports a test failure or success. If the results of the two
@racket[expr]s  are inexact numbers, the test passes as long
as the difference between the number is less than @racket[0.01].

The @racket[test/exn] form checks whether the @racket[expr] raises an
exception whose error message includes the string produced by
@racket[string-expr].

The @racket[test] and @racket[test/exn] forms have type @racket[Void],
although they do not actually produce a void value; instead, they
produce results suitable for automatic display through a top-level
expression, and the @racket[Void] type merely prevents your program
from using the result.

See also @racket[print-only-errors] and @racket[module+].}


@defform[(time expr)]{

Shows the time taken to evaluate @racket[expr] and returns the value
of @racket[expr].}


@defform[(let/cc id expr)]{

Equivalent to @racket[(call/cc (lambda (id) expr))].}


@; ----------------------------------------

@section{Predefined Functions and Constants}


@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Booleans}

@defthing[not (Boolean -> Boolean)]{

Boolean negation.

@examples[#:eval demo
(not #t)]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Lists}

@deftogether[(
@defthing[empty (Listof 'a)]
@defthing[empty? ((Listof 'a) -> Boolean)]
@defthing[cons ('a (Listof 'a) -> (Listof 'a))]
@defthing[cons? ((Listof 'a) -> Boolean)]
@defthing[first ((Listof 'a) -> 'a)]
@defthing[rest ((Listof 'a) -> (Listof 'a))]
)]{

Essential list primitives: a list is either @racket[empty] or
@racket[cons] of an item onto a list. The @racket[empty?] predicate
recognizes the empty list, a @racket[cons] recognizes any other list.
The @racket[first] and @racket[rest] functions select back out the two
arguments to @racket[cons].

@examples[#:eval demo
empty
(cons 1 empty)
(first (cons 1 empty))
(rest (cons 1 empty))
(define my-list (cons 1 (cons 2 (cons 3 empty))))
my-list
(first my-list)
(rest my-list)
(first (rest my-list))
(define also-my-list (list 1 2 3))
also-my-list
(rest also-my-list)
]}


@deftogether[(
@defthing[second ((Listof 'a) -> 'a)]
@defthing[third ((Listof 'a) -> 'a)]
@defthing[fourth ((Listof 'a) -> 'a)]
@defthing[list-ref ((Listof 'a) Number -> 'a)]
)]{

Shorthands for accessing the @racket[first] of the @racket[rest] of a
list, and so on. The second argument to @racket[list-ref] specifies
the number of @racket[rest]s to use before a @racket[first], so it
effectively counts list items from @racket[0].

@examples[#:eval demo
(define my-list (list 1 2 3))
(second my-list)
(list-ref my-list 2)
]}


@defthing[length ((Listof 'a) -> Number)]{

Returns the number of items in a list.

@examples[#:eval demo
(define my-list (cons 1 (cons 2 (cons 3 empty))))
(length my-list)
]}

@defthing[append ((Listof 'a) (Listof 'a) -> (Listof 'a))]{

Produces a list that has the items of the first given list followed by
the items of the second given list.

@examples[#:eval demo
(define my-list (list 1 2 3))
(define my-other-list (list 3 4 5))
(append my-list my-other-list)
(append my-other-list my-list)
]}

@defthing[reverse ((Listof 'a) -> (Listof 'a))]{

Returns a list that has the same elements as the given one, but in
reverse order.

@examples[#:eval demo
(reverse (list 1 2 3))]}


@defthing[member ('a (Listof 'a) -> Boolean)]{

Determines whether a value is an item in a list. Item are compared
using @racket[equal?].

@examples[#:eval demo
(member 1 (list 1 2 3))
(member 4 (list 1 2 3))]}


@defthing[map (('a -> 'b) (Listof 'a) -> (Listof 'b))]{

Applies a function in order to each element of a list and forms a new
list with the results.

@examples[#:eval demo
(map add1 (list 1 2 3))
(map to-string (list 1 2 3))]}


@defthing[map2 (('a 'b -> 'c) (Listof 'a) (Listof 'b) -> (Listof 'c))]{

Applies a function in order to each pair of elements from two lists in
``parallel,'' forming a new list with the results. An exception is
raised if the two lists have different lengths.

@examples[#:eval demo
(map2 + (list 1 2 3) (list 3 4 5))]}


@defthing[filter (('a -> Boolean) (Listof 'a) -> (Listof 'a))]{

Returns a list containing (in order) the items of a given list for
which a given function returns true.

@examples[#:eval demo
(filter even? (list 1 2 3 4))
(filter odd? (list 1 2 3 4))
]}


@deftogether[(
@defthing[foldl (('a 'b -> 'b) 'b (Listof 'a) -> 'b)]
@defthing[foldr (('a 'b -> 'b) 'b (Listof 'a) -> 'b)]
)]{

Applies a function to an accumulated value and each element of a list,
each time obtaining a new accumulated value. The second argument to
@racket[foldl] or @racket[foldr] is the initial accumulated value, and
it is provided as the first argument in each call to the given
function. While @racket[foldl] applies the function or items in the
list from from to last, @racket[foldr] applies the function or items
in the list from last to first.

@examples[#:eval demo
(foldl + 10 (list 1 2 3))
(foldl (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
(foldr (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
]}


@defthing[build-list (Number (Number -> 'a) -> (Listof 'a))]{

Creates a list of items produced by calling a given function on
numbers starting from @racket[0] a given number of times.

@examples[#:eval demo
(build-list 5 (lambda (v) (* v 10)))
]}
 

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Numbers}

@deftogether[(
@defthing[+ (Number Number -> Number)]
@defthing[- (Number Number -> Number)]
@defthing[* (Number Number -> Number)]
@defthing[/ (Number Number -> Number)]
@defthing[modulo (Number Number -> Number)]
@defthing[remainder (Number Number -> Number)]
@defthing[min (Number Number -> Number)]
@defthing[max (Number Number -> Number)]
@defthing[floor (Number -> Number)]
@defthing[ceiling (Number -> Number)]
@defthing[add1 (Number -> Number)]
@defthing[sub1 (Number -> Number)]
)]{

Standard arithmetic functions.

@examples[#:eval demo
(+ 1 2)
(- 10 9)
(/ 10 5)
(modulo 10 3)
(remainder 10 3)
(min 1 2)
(max 1 2)
(floor 10.1)
(ceiling 10.1)
(ceiling 10.1)
(add1 10)
(sub1 10)
]}

@deftogether[(
@defthing[= (Number Number -> Boolean)]
@defthing[> (Number Number -> Boolean)]
@defthing[< (Number Number -> Boolean)]
@defthing[>= (Number Number -> Boolean)]
@defthing[<= (Number Number -> Boolean)]
@defthing[zero? (Number -> Boolean)]
@defthing[odd? (Number -> Boolean)]
@defthing[even? (Number -> Boolean)]
)]{

Standard Number comparisons and predicates.

@examples[#:eval demo
(= 1 1)
(> 1 2)
(< 1 2)
(zero? 1)
(odd? 1)
(even? 1)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Symbols}

@defthing[symbol=? (Symbol Symbol -> Boolean)]{

Compares symbols

@examples[#:eval demo
(symbol=? 'apple 'apple)
(symbol=? 'apple 'Apple)
]}

@deftogether[(
@defthing[string->symbol (String -> Symbol)]
@defthing[symbol->string (Symbol -> String)]
)]{

Converts between symbols and strings.

@examples[#:eval demo
(string->symbol "apple")
(symbol->string 'apple)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Strings}

@deftogether[(
@defthing[string=? (String String -> Boolean)]
@defthing[string-append (String String -> String)]
@defthing[string-length (String -> Number)]
@defthing[substring (String Number Number -> String)]
@defthing[string-ref (String Number -> Char)]
)]{

Standard string primitives.

@examples[#:eval demo
(string=? "apple" "apple")
(string-append "apple" "banana")
(string-length "apple")
(substring "apple" 1 3)
(string-ref "apple" 0)
]}


@defthing[to-string ('a -> String)]{

Converts any value to a printed form as a string.

@examples[#:eval demo
(to-string 1)
(to-string 'two)
(to-string "three")
(to-string (list 1 2 3))
(to-string `(1 two "three"))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Characters}

@defthing[char=? (Char Char -> Boolean)]{

Compares characters.

@examples[#:eval demo
(char=? #\a #\b)
]}


@deftogether[(
@defthing[string->list (String -> (Listof Char))]
@defthing[list->string ((Listof Char) -> String)]
)]{

Converts between a string and a list of characters.

@examples[#:eval demo
(string->list "apple")
(list->string (list #\a #\b #\c))
]}


@; - - - - - - - - - - - - - - - - - - - - -
@subsection{S-Expressions}

@tutorial["s-exp-tutorial"]

A @deftech{S-expression} typically represents program text. For example,
placing a @litchar{'} in from of any @racketmodname[plait]
expression (which is the same as wrapping it with @racket[quote])
creates an S-expression that contains the identifiers (as symbols),
parenthesization (as lists), and other constants as the expression
text. Various @racketmodname[plait] values, including symbols, numbers,
and lists, can be coerced to and from S-expression form.

The representation of an S-expression always reuses some other
@racketmodname[plait] value, so conversion to and from an S-expression
is a kind cast. For example, the @racket[s-exp-symbol?] function
determines whether an S-expression embeds an immediate symbol; in that
case, @racket[s-exp->symbol] extracts the symbol, while any other
value passed to @racket[s-exp->symbol] raises an exception. The
@racket[symbol->s-exp] function wraps a symbol as an S-expression.

@margin-note{For interoperability of @tech{S-expressions} with untyped
Racket programs, see @racket[s-exp-content] and @racket[s-exp].}

@(define-syntax-rule (converter what s-exp-X? s-exp->X X->s-exp)
  @elem{
   Checks whether an @tech{S-expression} corresponds to a single symbol
   and casts it from or to such a form. If @racket[s-exp->X] is
   given an S-expression for which @racket[s-exp-X?] returns false,
   then @racket[s-exp->X] raises an exception.})

@deftogether[(
@defthing[s-exp-symbol? (S-Exp -> Boolean)]
@defthing[s-exp->symbol (S-Exp -> Symbol)]
@defthing[symbol->s-exp (Symbol -> S-Exp)]
)]{

@converter[@elem{a single symbol} s-exp-symbol? s-exp->symbol symbol->s-exp]

@examples[#:eval demo
(s-exp-symbol? `apple)
(s-exp->symbol `apple)
(eval:error (s-exp->symbol `1))
(symbol->s-exp 'apple)
]}

@deftogether[(
@defthing[s-exp-number? (S-Exp -> Boolean)]
@defthing[s-exp->number (S-Exp -> Number)]
@defthing[number->s-exp (Number -> S-Exp)]
)]{

@converter[@elem{a single number} s-exp-number? s-exp->number number->s-exp]

@examples[#:eval demo
(s-exp-number? `1)
(s-exp->number `1)
(number->s-exp 1)
]}

@deftogether[(
@defthing[s-exp-string? (S-Exp -> Boolean)]
@defthing[s-exp->string (S-Exp -> String)]
@defthing[string->s-exp (String -> S-Exp)]
)]{

@converter[@elem{a single string} s-exp-string? s-exp->string string->s-exp]

@examples[#:eval demo
(s-exp-string? `"apple")
(s-exp->string `"apple")
(string->s-exp "apple")
]}

@deftogether[(
@defthing[s-exp-boolean? (S-Exp -> Boolean)]
@defthing[s-exp->boolean (S-Exp -> Boolean)]
@defthing[boolean->s-exp (Boolean -> S-Exp)]
)]{

@converter[@elem{a single boolean} s-exp-boolean? s-exp->boolean boolean->s-exp]

@examples[#:eval demo
(s-exp-boolean? `#f)
(s-exp->boolean `#f)
(boolean->s-exp #f)
(s-exp-boolean? `false)
(s-exp-symbol? `false)
]}

@deftogether[(
@defthing[s-exp-list? (S-Exp -> Boolean)]
@defthing[s-exp->list (S-Exp -> (Listof S-Exp))]
@defthing[list->s-exp ((Listof S-Exp) -> S-Exp)]
)]{

@converter[@elem{an immediate list} s-exp-list? s-exp->list list->s-exp]
A list produced by @racket[s-exp->list] always contains S-expression items.

@examples[#:eval demo
(s-exp-list? `(1 2 3))
(s-exp-list? `1)
(s-exp->list `(1 2 3))
(list->s-exp (list `1 `2 `3))
(eval:error (list->s-exp (list 1 2 3)))
]}

@defthing[s-exp-match? (S-Exp S-Exp -> Boolean)]{

@tutorial["s-exp-match-tutorial"]

Compares the first S-expression, a @deftech{pattern}, to the second
S-expression, a @deftech{target}.

To a first approximation, @racket[s-exp-match?] is just
@racket[equal?] on the two S-expressions. Unlike @racket[equal?],
however, certain symbols in the @tech{pattern} and can match various
S-expressions within the @tech{target}.

For example, @racket[`NUMBER] within a @tech{pattern} matches
any number in corresponding position within the @tech{target}:

@examples[#:eval demo
(s-exp-match? `(+ NUMBER NUMBER) `(+ 1 10))
(s-exp-match? `(+ NUMBER NUMBER) `(+ 1 x))
(s-exp-match? `(+ NUMBER NUMBER) `(- 1 10))
]

The following symbol S-expressions are treated specially within
the @tech{pattern}:

@itemlist[

 @item{@racket[`NUMBER] --- matches any number S-expression}

 @item{@racket[`STRING] --- matches any string S-expression}

 @item{@racket[`SYMBOL] --- matches any symbol S-expression}

 @item{@racket[`ANY] --- matches any S-expression}

 @item{@racket[`...] --- within a list S-expression, matches any
       number of repetitions of the preceding S-expression within the
       list; only one @racket[`...] can appear as an immediate element
       of a pattern list, and @racket[`...] is not allowed within a
       pattern outside of a list or as the first element of a list}

]

Any other symbol in a @tech{pattern} matches only itself in the
@tech{target}. For example, @racket[`+] matches only @racket[`+].

@examples[#:eval demo
(s-exp-match? `NUMBER `10)
(s-exp-match? `NUMBER `a)
(s-exp-match? `SYMBOL `a)
(s-exp-match? `SYMBOL `"a")
(s-exp-match? `STRING `"a")
(s-exp-match? `STRING `("a"))
(s-exp-match? `ANY `("a"))
(s-exp-match? `ANY `10)
(s-exp-match? `any `10)
(s-exp-match? `any `any)

(s-exp-match? `(SYMBOL) `(a))
(s-exp-match? `(SYMBOL) `(a b))
(s-exp-match? `(SYMBOL SYMBOL) `(a b))
(s-exp-match? `((SYMBOL) SYMBOL) `((a) b))
(s-exp-match? `((SYMBOL) NUMBER) `((a) b))
(s-exp-match? `((SYMBOL) NUMBER ((STRING))) `((a) 5 (("c"))))
(s-exp-match? `(lambda (SYMBOL) ANY) `(lambda (x) x))
(s-exp-match? `(lambda (SYMBOL) ANY) `(function (x) x))

(s-exp-match? `(SYMBOL ...) `(a b))
(s-exp-match? `(a ...) `(a b))
(s-exp-match? `(a ...) `(a a))
(s-exp-match? `(a ...) `())
(s-exp-match? `(a ... b) `())
(s-exp-match? `(a ... b) `(b))
(s-exp-match? `(a ... b) `(a a a b))
(s-exp-match? `((a ...) b ...) `((a a a) b b b b))
(s-exp-match? `((a ...) b ...) `((a a a) b c b b))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Vector}

@tutorial["state-tutorial"]

@deftogether[(
@defthing[make-vector (Number 'a -> (Vectorof 'a))]
@defthing[vector-ref ((Vectorof 'a) Number -> 'a)]
@defthing[vector-set! ((Vectorof 'a) Number 'a -> Void)]
@defthing[vector-length ((Vectorof 'a) -> Number)]
)]{

A @deftech{vector} is similar to a list, but it supports constant-time access to
any item in the vector and does not support constant-time extension.
In addition, vectors are mutable.

The @racket[make-vector] function creates a vector of a given size and
initializes all vector items to a given value. The @racket[vector-ref]
function accesses the value in a vector slot, and @racket[vector-set!]
changes the value in a slot. The @racket[vector-length] function
reports the number of slots in the vector.

@examples[#:eval demo
(define vec (make-vector 10 "apple"))
(vector-length vec)
(vector-ref vec 5)
(vector-set! vec 5 "banana")
(vector-ref vec 5)
(vector-ref vec 6)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Boxes}

@tutorial["state-tutorial"]

@deftogether[(
@defthing[box ('a -> (Boxof 'a))]
@defthing[unbox ((Boxof 'a) -> 'a)]
@defthing[set-box! ((Boxof 'a) 'a -> Void)]
)]{

A @deftech{box} is like a vector with a single slot. Boxes are used primarily to
allow mutation. For example, the value of a field in a variant
instance cannot be modified, but the field's value can be a box, and
then the box's content can be modified.

The @racket[box] function creates a box with an initial value for its
slot, @racket[unbox] accesses the current value in a box's slot, and
@racket[set-box!] changes the value.

@examples[#:eval demo
(define bx (box "apple"))
(define bx2 bx)
(unbox bx)
(set-box! bx "banana")
(unbox bx)
(unbox bx2)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Tuples}

@tutorial["tuples-tutorial"]

@deftogether[(
@defthing[pair ('a 'b -> ('a * 'b))]
@defthing[fst (('a * 'b) -> 'a)]
@defthing[snd (('a * 'b) -> 'b)]
)]{

Shorthands for two-element tuples: the @racket[pair] function creates
a tuple, and the @racket[fst] and @racket[snd] functions access tuple
items.

@examples[#:eval demo
(define p (pair 1 "apple"))
p
(fst p)
(snd p)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Optional Values}

@tutorial["tuples-tutorial"]

@deftogether[(
@defthing[none (-> (Optionof 'a))]
@defthing[some ('a -> (Optionof 'a))]
@defthing[some-v ((Optionof 'a) -> 'a)]
@defthing[none? ((Optionof 'a) -> bool)]
@defthing[some? ((Optionof 'a) -> bool)]
)]{

Defined as
@racketblock[
(define-type (Optionof 'a)
  (none)
  (some [v : 'a]))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Hash Tables}

@deftogether[(
@defthing[make-hash ((Listof ('a * 'b)) -> (Hashof 'a 'b))]
@defthing[hash ((Listof ('a * 'b)) -> (Hashof 'a 'b))]
@defthing[hash-ref ((Hashof 'a 'b) 'a -> (Optionof 'b))]
)]{

The @racket[make-hash] function creates a mutable hash table that is
initialized with a given mapping of keys to values (as a list of
tuples pairing keys to values). The @racket[hash] function similarly
creates an immutable hash table that supports constant-time functional
update.

The @racket[hash-ref] function works on either kind of hash table to
find the value for a given key. If the hash table contains a mapping
for a given key, @racket[hash-ref] returns the key's value wrapped
with @racket[some]. Otherwise, @racket[hash-ref] returns
@racket[(none)].

@examples[#:eval demo
(define m-ht (make-hash (list (pair 1 "apple") (pair 2 "banana"))))
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref m-ht 1)
(hash-ref i-ht 1)
(hash-ref m-ht 3)
]}

@deftogether[(
@defthing[hash-set! ((Hashof 'a 'b) 'a 'b -> Void)]
@defthing[hash-remove! ((Hashof 'a 'b) 'a -> Void)]
)]{

Changes the mapping of a mutable hash table. The @racket[hash-set!]
operation adds or changes the value for a given key, while
@racket[hash-remove!] deletes the mapping (if any) of a given key.

Providing an immutable hash table triggers an exception.

@examples[#:eval demo
(define m-ht (make-hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref m-ht 1)
(hash-ref m-ht 3)
(hash-set! m-ht 3 "coconut")
(hash-set! m-ht 1 "Apple")
(hash-ref m-ht 1)
(hash-ref m-ht 3)
]}

@deftogether[(
@defthing[hash-set ((Hashof 'a 'b) 'a 'b -> (Hashof 'a 'b))]
@defthing[hash-remove ((Hashof 'a 'b) 'a -> (Hashof 'a 'b))]
)]{

Produces an immutable hash table that is like a given one, but with a
mapping changed, added, or removed. The @racket[hash-set] operation
adds or changes the value for a given key in the result hash table, while
@racket[hash-remove] deletes the mapping (if any) of a given key
in the result hash table.

@examples[#:eval demo
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref i-ht 1)
(define i-ht2 (hash-set (hash-set i-ht 1 "Apple")
                        3 "coconut"))
(hash-ref i-ht2 1)
(hash-ref i-ht2 3)
(hash-ref i-ht 3)
]}

@defthing[hash-keys ((Hashof 'a 'b) -> (Listof 'a))]{

Returns the keys mapped by a hash table, which can be mutable or
immutable.

@examples[#:eval demo
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-keys i-ht)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Parameters}

@deftogether[(
@defthing[make-parameter ('a -> (Parameterof 'a))]
@defthing[parameter-ref ((Parameterof 'a) -> 'a)]
@defthing[parameter-set! ((Parameterof 'a) 'a -> Void)]
)]{

A parameter implements a kind dynamic binding. The
@racket[make-parameter] function creates a fresh parameter,
@racket[parameter-ref] accesses the parameter's current value, and
@racket[parameter-set!] changes the parameter's current value (i.e.,
at the nearest dynamic binding established with @racket[parameterize],
if any).

See also @racket[parameterize].}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Equality}

@deftogether[(
@defthing[equal? ('a 'a -> Boolean)]
)]{

Compares two values for equality. Roughly, two values are
@racket[equal?] when they print the same, but opaque values such as
functions are @racket[equal?] only if they are @racket[eq?].

@examples[#:eval demo
(equal? "apple" "apple")
(equal? (values 1 'two "three") (values 1 'two "three"))
]}


@defthing[eq? ('a 'a -> Boolean)]{

Checks whether two values are exactly the same value, which amounts to
checking pointer equality. The @racket[eq?] function is well-defined
on symbols (where it is equivalent to @racket[equal?]), mutable data
structures (where pointer equality corresponds to shared mutation),
and the result of a single expression (such as comparing a
identifier's value to itself), but it should be avoided otherwise.

@examples[#:eval demo
(eq? 'apple 'apple)
(let ([get-one (lambda () 1)])
  (eq? get-one get-one))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Other Functions}

@defthing[identity ('a -> 'a)]{Identity primitive.}

@defthing[error (Symbol String -> 'a)]{Error primitive.}

@defthing[display ('a -> Void)]{Output primitive.}

@defthing[read (-> S-Exp)]{Input primitive.}

@defthing[void (-> Void)]{Void primitive.}

@defthing[print-only-errors (Boolean -> Void)]{

Enables or disables the printing of tests that pass. Tests that fail
always cause printing.}

@defthing[call/cc ((('a -> 'b) -> 'a) -> 'a)]{

Passes the current continuation to the given function, and returns
that function's result.

The current continuation is itself represented as a function. Applying
a continuation function discards the current continuation and replaces
it with the called one, supplying the given value to that
continuation.}

@deftogether[(
@defthing[s-exp-content @#,italic{no type}]
@defthing[s-exp @#,italic{no type}]
)]{

These functions have no type, so they cannot be used in a
@racketmodname[plait] program. They can be used in untyped contexts to
coerce a @racketmodname[plait] @tech{S-expression} to a plain Racket
S-expression and vice-versa.}

@deftogether[(
@defthing[tuple-content @#,italic{no type}]
@defthing[tuple @#,italic{no type}]
)]{

These functions have no type, so they cannot be used in a
@racketmodname[plait] program. They can be used in untyped contexts to
coerce a @racketmodname[plait] @tech{tuple} to an immutable vector
and vice-versa.}

@; ----------------------------------------

@section{Types}

@deftogether[(
@defidform[Number]
@defidform[Boolean]
@defidform[Symbol]
@defidform[String]
@defidform[Char]
@defidform[S-Exp]
@defidform[Void]
)]{Primitive types.}

@defform[#:id -> (type ... -> type)]{

Type for functions. Each @racket[type] before the @racket[->]
corresponds to a function argument, and the @racket[type] after
@racket[->] corresponds to a function result.}

@defform/none[#:literals (*) (type * ...+)]{

Type for @tech{tuples}. Each @racket[*]-separated @racket[type] is
the type of an element in the tuple.}

@defform/none[()]{

Type for the empty @tech{tuple}.}


@defform[(Listof type)]{Type for lists of elements, where @racket[type] is the type of one element.}
@defform[(Boxof type)]{Type for mutable boxes, where @racket[type] is the type of the box's content.}
@defform[(Vectorof type)]{Type for vectors of elements, where @racket[type] is the type of one element.}
@defform[(Parameterof type)]{Type for parameters, where @racket[type] is the type of the parameter's value.}

@defform[(Hashof type type)]{

Type for hash tables, where the first @racket[type] corresponds to
keys and the second @racket[type] correspond to values.}

@defform[(Optionof type)]{Defined as
@racketblock[
(define-type (Optionof 'a)
  (none)
  (some [v : 'a]))
]
and used, for example, for the result of @racket[hash-ref].}

@defform/none[#:literals (quote) (quote @#,racket[_id])]{

A type variable that stands for an unspecified type. The type checker
effectively replaces each type variable with some other type---but
possibly another type variable that is bound by an inferred
polymorphic type.

In the following example, the type checker determines that @racket['a]
must be replaced with @racket[Number]:

@examples[#:eval demo #:label #f
(define one : 'a 1)
one]

In the following example, the type checker determines that @racket['b]
stands for the argument type of a polymorphic function:

@examples[#:eval demo2 #:label #f
(define (f [x : 'b]) x)
]

In the following examples, the type checker is unable to replace
@racket['a] consistently with the same type everywhere:

@examples[#:eval demo #:label #f
(eval:error (if (has-type #t : 'a) (has-type 1 : 'a) 2))
(eval:error (define x : 'a (list (has-type 1 : 'a))))
]

Multiple uses of the same type variable (i.e., with the same
@racket[_id]) are constrained to refer to the same type only when the
uses have the same scope. A type variable's scope is determined as
follows:

@itemlist[

 @item{When a type variable is encountered in a left-to-right parsing
       of a program and no same-named variable is already in scope,
       then the variable's scope is set to the nearest enclosing
       @racket[define] form, @racket[define-type] form,
       @racket[lambda] form, @racket[let] right-hand side,
       @racket[letrec] right-hand side, or @racket[let*] right-hand
       side.}

 @item{When a type variable is encountered in a left-to-right parsing
       of a program and some same-named variable is already in scope,
       the variable have the same scope (and are confined to have the
       same meaning).}

]

For example, type variables introduced in separate definitions are
always distinct, so in the following example, the first @racket['a]
can stand for @racket[Number] while the second stands for
@racket[String]:

@examples[#:eval demo #:label #f
(define one : 'a 1)
(define two : 'a "two")
]

A type variable used for a @racket[lambda] argument is scoped to the
entire @racket[lambda] form, so the uses of @racket['a] in the definitions of
the following example refer back to that argument type, and the argument
cannot have both @racket[Number] and @racket[String] type:

@examples[#:eval demo #:label #f
(eval:error
 (lambda ([x : 'a])
   (local [(define one : 'a 1)
           (define two : 'a "two")]
     #f)))
]

Beware that the order of expressions can affect the scope of type
variables within the expressions:

@examples[#:eval demo #:label #f
(values
  (has-type 1 : 'a)
  (letrec ([f (lambda ([x : 'a]) x)]) f))
(values
  (letrec ([f (lambda ([x : 'a]) x)]) f)
  (has-type 1 : 'a))
]}
  

@; ----------------------------------------

@section{Syntactic Literals}

@deftogether[(
@defidform[typed-in]
@defidform[opaque-type-in]
@defidform[:]
)]{

Syntactic literals are for use in declarations such as @racket[define]
and @racket[require]; see @racket[define] and @racket[require] for
more information. A @racket[:] can also be used for a type declaration
within a definition sequence; see @secref["Definitions"].}

@; ----------------------------------------

@section{Type Checking and Inference}

Type checking and inference is just as in ML (Hindley-Milner), with
a few small exceptions:

@itemize[

 @item{Functions can take multiple arguments, instead of requring a @tech{tuple}
   of arguments. Thus, @racket[(Number Number -> Number)] is a different type
   than either @racket[((Number * Number) -> Number)], which is the tuple
   variant, or @racket[(Number -> (Number -> Number))], which is the curried
   variant.}

 @item{Since all top-level definitions are in the same
   mutually-recursive scope, the type of a definition's right-hand
   side is not directly unified with references to the defined
   identifier on the right-hand side. Instead, every reference to an
   identifier---even a reference in the identifier's definition---is
   unified with a instantiation of a polymorphic type inferred for the
   definition.

   Compare OCaml:

@verbatim[#:indent 2]{
       # let rec f = fun x -> x
             and h = fun y -> f 0 
             and g = fun z -> f "x";;
       This expression has type string but is here used with type int
}

    with

@verbatim[#:indent 2]{
       (define (f x) x)
       (define (h y) (f 0))
       (define (g y) (f "x"))
       ; f : ('a -> 'a)
       ; h : ('a -> Number)
       ; g : ('a -> String)
}

   A minor consequence is that polymorphic recursion (i.e., a self
   call with an argument whose type is different than that for the
   current call) is allowed. Recursive types, however, are prohibited.
   Polymorphic recursion is not decidable, so see @racket[#:fuel] in
   @secref["untyped-and-lazy"].

   The usual value restriction applies for inferring polymorphic
   types, where expression matching the following grammar
   (@emph{before} macro expansion, unfortunately) are considered
   values:

   @racketgrammar[
      #:literals (lambda list values cons empty quote)
      value-expr (lambda (id/ty ...) expr)
                 (lambda (id/ty ...) : type expr) 
                 (values value-expr ...)
                 (list value-expr ...)
                 empty
                 (cons value-expr value-expr)
                 (hash value-expr ...)
                 (variant-id value ...)
                 (quote datum)
                 id
                 string
                 character
                 number
                 boolean
   ]

   where @racket[_variant-id] is @racket[none], @racket[some],
   or a constructor bound by @racket[define-type].}

 @item{Variables are mutable when @racket[set!] is used, but
  assignment via @racket[set!] is disallowed on a variable after a
  polymorphic type has been inferred for it (e.g., in an interaction
  after type checking is complete).}

 @item{Since all definitions are recursively bound, and since the
   right-hand side of a definition does not have to be a function, its
   possible to refer to a variable before it is defined. The type
   system does not prevent ``reference to identifier before
   definition'' errors.}

 @item{Interactive evaluation (e.g., in DrRacket's interactions
   window) can redefine identifiers that were previously defined
   interactively or that were defined in a module as mutable.
   Redefinition cannot change the identifier's type. Due to a
   limitation of the type checker, identifiers of polymorphic type
   cannot be redefined or redeclared. Type declarations are allowed in
   interactive evaluation, but a declared type is never treated as a
   polymorphic type.}

]

When typechecking fails, the error messages reports and highlights (in
pink) all of the expressions whose type contributed to the
failure. That's often too much information. As usual, explicit type
annotations can help focus the error message.

@; ----------------------------------------

@section[#:tag "untyped-and-lazy"]{Untyped, Lazy, and Fuel Modes}

Use @racket[#:untyped] immediately after @racket[@#,hash-lang[]
@#,racketmodname[plait]] to disable type checking. The syntax of a
@racketmodname[plait] module is the same with and without
@racket[#:untyped], but types are ignored when @racket[#:untyped] is
specified. An untyped Plait module can interoperate with a typed Plait
module, and dynamic checks are inserted at the boundary to protect
typed functions from abuse by untyped code.

Use @racket[#:lazy] immediately after @racket[@#,hash-lang[]
@#,racketmodname[plait]] to switch evaluation to lazy mode. The syntax
and type system are unchanged, but argument expressions for function
calls are evaluated only when forced (by a test or by printing,
ultimately). A lazy Plait module will not interoperate well with an
eager module.

Use @racket[#:fuel _amount] immediately after @racket[@#,hash-lang[]
@#,racketmodname[plait]] to specify how much effort should be spent
resolving potentially cyclic dependencies due to inference of
polymorphic recursion. The default fuel is @racket[100].

The @racket[#:untyped], @racket[#:lazy], and @racket[#:fuel] modifiers
can be combined, and the combination can be declared in any order.

@; ----------------------------------------

@close-eval[demo]
@close-eval[demo2]
