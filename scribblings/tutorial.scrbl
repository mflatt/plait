#lang scribble/manual
@(require (for-label (only-meta-in 0 plait))
          racket/runtime-path
          scribble/examples
          scribble/core
          scribble/html-properties)

@(define plait-eval (make-base-eval))
@examples[#:hidden #:eval plait-eval (require plait)]
@(define-syntax-rule (interaction e ...)
   @examples[#:label #f #:eval plait-eval e ...])

@(define ref.scrbl '(lib "scribblings/reference/reference.scrbl"))

@(define (pink e)
   @elem[#:style (style #f (list (background-color-property "pink"))) e])

@(define-runtime-path demo.rkt "demo.rkt")

@title[#:style 'toc]{Tutorial}

@margin-note{For a quick refresher of the tutorial content, try
@elem[#:style (style #f (list (link-resource
demo.rkt)))]{@filepath{demo.rkt}}.}

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "getting-started"]{Getting Started}

To get started with Plait,
@link["https://download.racket-lang.org/"]{download Racket}, install
it, start @onscreen{DrRacket}, and install the @tt{plait} package using DrRacket's
@onscreen{Install Package...} menu item in the @onscreen{File} menu.

Then, in the top part of the DrRacket window, type

@racketmod[plait]

and click the @onscreen{Run} button (or use the keyboard shortcut
shown in the @onscreen{Racket} menu). In the bottom part of the window, type
@racket[1] and hit Return, and you should see this result:

@interaction[1]

In other words, the expression @racket[1] has type @racket[Number],
and the value of the expression is @racketresultfont{1}.

In this tutorial, we will mostly show expressions as if typed in that
bottom area. You can also put the expressions in the top area and hit
@onscreen{Run} again, but in that case, the type of the result will
not print before the result.

In a few places, the tutorial shows lines that include a semicolon,
@litchar{;}. A semicolon starts a comment that continues to the end of
the line.

@; ----------------------------------------
@section[#:tag "data-tutorial"]{Simple Data}

Plait supports various kinds of numbers, all with type @racket[Number]:

@interaction[1 0.5 1/2 1+2i]

The number @racket[1/2] is a literal number, not a division operation.
Similarly, @racket[1+2i] is a literal complex number. The full syntax
of numbers is probably not important, but it's @seclink[#:doc
ref.scrbl "parse-number"]{Racket's number syntax}.

The booleans @defterm{true} and @defterm{false} are written
@racket[#t] and @racket[#f], but you can also write them
@racket[#true] and @racket[#false]:

@interaction[#t #f #true #false]

Strings are written the usual way with a starting and ending @litchar{"}:

@interaction["apple" "banana cream pie"
             "yes, \"escape\" quotes with backslash"]

In addition to strings, Plait includes string-like values called
@defterm{symbols}. A symbol is written with a single quote @litchar{'}
followed by a sequence of non-whitespace characters.

@interaction[
'apple
'banana-cream-pie
'a->b
'#%$^@*&?!
]

Almost any non-whitespace character is allowed in a symbol, except for
the following characters:

@t{
  @hspace[2] @litchar{(} @litchar{)} @litchar{[} @litchar{]}
  @litchar["{"] @litchar["}"]
  @litchar{"} @litchar{,} @litchar{'} @litchar{`}
  @litchar{;}
}

Characters like @litchar{-}, @litchar{>}, and @litchar{?} are not only
allowed in symbols, they are frequently used that way. The @litchar{|}
and @litchar{\} characters are allowed, but they're treated as quoting
and escaping characters, so don't use them.

Individual characters are infrequently used in Plait, but they're
written with @litchar{#\}:

@interaction[
#\a
#\b
#\A
(code:line #\space (code:comment @#,t{same as @litchar{#\} followed by a space}))
]

@; ----------------------------------------
@section[#:tag "built-ins-tutorial"]{Using Built-in Functions}

Plait includes some of the usual functions on numbers, like
@racket[floor] and @racket[max]. To call a function, start with an
open parenthesis, then use the function name, then the argument, and
finally a closing parenthesis:

@interaction[
(floor 1.2)
(max 3 5)
]

The parenthesis must be @italic{@bold{before the function}}, not
after. Don't use commas between arguments. Also, @italic{@bold{extra
parentheses are not allowed}}. If you include extra parentheses around
@racket[3], for example, then Plait will complain that @racket[3] is
not a function, since the parentheses mean a function call:

@interaction[
(eval:alts (max (@#,pink[@racket[3]]) 5) (eval:error (max (3) 5)))
]

The same error happens if you add parentheses around the call to
@racket[max], since the result of @racket[max] is a number:

@interaction[
(eval:alts (@#,pink[@racket[(max 3 5)]]) (eval:error ((max 3 5))))
]

The type of a function is written with @racket[->] in parentheses. The
function's argument types appear before the arrow, and the function's
result type is after the arrow. A function is a value, so if you
evaluate just @racket[max] without calling it, then Plait will show
the type and print that the result is a procedure (which is synonymous
with ``function'' in Plait):

@interaction[
max
]

Unlike most languages, arithmetic operations such as @racket[+] and
@racket[*] in Plait are just functions, and they are called the same
way as other functions---just after an open parenthesis, and grouped
with their arguments by a closing parenthesis:

@interaction[
(+ 3 5)
(* 3 5)
+
]

Note that @racket[+] is allowed as a function name in fundamentally
the same way that @litchar{+} is allowed in the symbol @racket['+].

If you try to put the operator-as-function in the middle of its
arguments, Plait will complain that the first argument isn't a
function, because the opening parenthesis means that first thing in
the parenthesis should be a function to call:

@interaction[
(eval:alts (@#,pink[@racket[3]] + 5) (eval:error (3 + 5)))
]

If you omit then parentheses, then Plait see three separate
expressions:

@racketblock[
@#,tt{>} + 3 5
@#,racketoutput{- (Number Number -> Number)}
@#,racketresultfont{#<procedure:+>}
@#,racketoutput{- Number}
@#,racketresultfont{3}
@#,racketoutput{- Number}
@#,racketresultfont{5}
]

The style of syntax that puts a function/operation name up front and
grouped with its arguments in parentheses is called
@defterm{parenthesized prefix notation}.

Treating @racket[+] like any other function makes Plait simpler, as
does using parenthesized prefix notation. Since you didn't have to
create Plait, you may not care that Plait is simpler this way. But if
you're building your own interpreter in a class that's about
programming languages, then Plait's regularity turns out to be a
convenient design to imitate; you can spend more time studying the
meaning of programming constructs and less time worrying about the
syntax of those constructs.

Here are some example uses of other built-in functions, and you can
click on any of the function names her eto jump to the documentation:

@interaction[
(not #t)
(not #f)
(+ 1 2)
(- 1 2)
(* 1 2)
(< 1 2)
(> 1 2)
(= 1 2)
(<= 1 2)
(>= 1 2)
(string-append "a" "b")
(string=? "a" "b")
(string-ref "a" 0)
(string=? "apple" (string-append "a" "pple"))
(equal? "apple" (string-append "a" "pple"))
(eq? 'apple 'apple)
(eq? 'apple 'orange)
]

Note that some operations work on multiple types. For example,
@racket[equal?] works on any two values, as long as the two values
have the same type. That flexibility and constraint is reflected in
the type of @racket[equal?] by a symbol placeholder @racket['a], which
you can read as ``a type to be picked later.'' A specific type is
picked for every individual use of @racket[equal?]:

@interaction[
(equal? 1 1)
(equal? "one" "one")
equal?
(eval:error (equal? 1 "one"))
]

@; ----------------------------------------
@section[#:tag "cond-tutorial"]{Conditionals}

The @racket[if] form works in the usual way, and it follows the
parenthesized-prefix convention of being grouped with its
subexpressions with parentheses:

@interaction[
(if (equal? "apple" "banana")
    'yes
    'no)
]

The line breaks above don't matter to Plait, but readers of your
programs will appreciate it if you normally put the ``then'' and
``else'' branches on their own lines and correctly intent them. The
correct indentation is the indentation that DrRacket gives you
automatically when you hit Return after @racket[(equal? "apple"
"banana")]. If you ever need to reindent a region of code, you can
select the region and hit Tab.

The @racket[cond] form is a multi-way @racket[if]. A @racket[cond]
form has a sequence of clauses, where each clause has a ``question''
and a result expression. The result expression is used only when the
question produces true. The @racket[cond] form tries the clauses in
order, and as soon as it finds a true result from a question, it
produces the corresponding result. The last clause's question cal be
@racket[else] as a synonym for @racket[#t].

@interaction[
(cond
  [(< 2 1) 17]
  [(> 2 1) 18])
(cond
  [(< 2 1) (/ 1 0)] (code:comment @#,t{result expression skipped})
  [(> 2 1) 18])
(code:line
 (cond
   [#t 8]
   [#t (/ 1 0)]) (code:comment @#,t{second clause not reached}))
(cond
  [(< 3 1) 0]
  [(< 3 2) 1]
  [(< 3 3) 2]
  [(< 3 4) 3])
(cond
  [(eq? 'a 'b) 0]
  [(eq? 'a 'c) 1]
  [else 2])
]

Plait doesn't distinguish between square brackets @litchar{[} and
@litchar{]} and parentheses @litchar{(} and @litchar{)}, as long as
each opener and closer match. You could use parentheses instead of
square brackets in the above examples---but don't. Plait programmers
should use square brackets in specific places by convention to make
programs more readable. Follow the conventions that you see in this
tutorial.

The @racket[and] and @racket[or] forms are short-cicuiting, too, and
they work with any number of boolean subexpressions:

@interaction[
(and #t #t)
(and #t #f)
(and (< 2 1) #t)
(code:line (and (< 2 1) (zero? (/ 1 0))) (code:comment @#,t{second expression is not evaluated}))
(or #f #t)
(or #f #f)
(code:line (or (< 1 2) (zero? (/ 1 0))) (code:comment @#,t{second expression is not evaluated}))
(and #t #t #t #t)
(or #f #f #f)
]

@; ----------------------------------------
@section[#:tag "lists-tutorial"]{Lists}

Plait lists are @defterm{uniform}, meaning that all of the elements of
a list must have the same type. The @racket[list] form creates a list:

@interaction[
(list 1 2 (+ 3 4))
(list (string-append "a" "b") "c")
]

As you can see, the type of a list is written with @racket[Listof] and
then the type of the elements of the list. You also see that the
result is printed using @litchar{'}. You can use a @litchar{'} to
create a list, but only for literal-value content (i.e., no
subexpressions to evaluate):

@interaction[
'(1 2 7)
(eval:error '(1 2 (+ 3 4)))
]

To understand that last error message, start by observing that the
@litchar{'} for a literal list is the same as a the @litchar{'} for a
symbol. As it turns out, a @litchar{'} for a list implicitly
distributes the @litchar{'} to each element of the list. So,
@racket['(a b)] is equivalent to @racket[(list 'a 'b)]. It's also the
case that @racket['(1 2 7)] is equivalent to @racket[(list '1 '2 '7)],
because @litchar{'} has no effect on a number, boolean, or string:

@interaction[
'1
'#t
'"apple"
'(milk cookies)
'((pen paper) (rock scissors paper))
]

The expression @racket['(1 2 (+ 3 4))] fails because that's the same
as @racket[(list 1 2 '(+ 3 4))], and @racket['(+ 3 4)] fails because
it's the same as @racket[(list '+ 3 4)], but a list cannot mix a symbol
with numbers.

A list is immutable. That is, the value @racket['(1 2 3)] is as
unchanging as the numbers @racket[1], @racket[2], and @racket[3]
within the list. You can't change a list to add new elements to
it---but you can create a new list that is like the old one, except
that it has another element. The @racket[cons] function takes an
element and a list and ``adds'' the element to the front of the list,
creating a new list with all of the elements:

@interaction[
(cons 1 '(2 3))
(cons "apple" '("banana"))
]

The @racket[cons] operation is constant-time, because a list is
internally represented as a singly linked list, and @racket[cons]
simply creates a new cell that contains the new value and then points
to the existing list.

If you have two lists, instead of one element and a list, you can
combine the lists with @racket[append]:

@interaction[
(append '(1 2) '(3 4))
]

Don't confuse @racket[cons] and @racket[append]. The @racket[cons]
function takes an @defterm{element} and a list, while @racket[append]
takes a @defterm{list} and a list. That difference is reflected in
their types:

@interaction[
cons
append
]

Mixing them up will trigger a type error:

@interaction[
(eval:error (cons '(1) '(2 3)))
(eval:error (append 1 '(2 3)))
]

A list doesn't have to contain any values:

@interaction[
(list)
]

The empty list is so useful that it has a name: @racket[empty].
Although the @racket[list] form may seem fundamental, the true
list-construction primitives are @racket[empty] and @racket[cons], and
you can build up any other list using those:

@interaction[
empty
(cons "food" empty)
(cons "dog" (cons "food" empty))
]

The @racket[empty?] function determines whether a list is empty, and
@racket[cons?] determines whether a list has at least one item:

@interaction[
(empty? empty)
(empty? '())
(cons? (cons 1 '()))
(cons? '(1))
(cons? empty)
(empty? '(1))
]

The @racket[cons] operation constructs a new value from two pieces.
The @racket[first] and @racket[rest] operations are the opposite of
@racket[cons]. Given a value produced by @racket[cons], @racket[first]
returns the item that @racket[cons] added to the start of the list,
and @racket[rest] returns the list that @racket[cons] added to. More
generally, @racket[first] gets the first item from a list, and
@racket[rest] gets everything list in the list when the first argument
is removed.

@interaction[
(first (cons 1 '(2 3)))
(rest (cons 1 '(2 3)))
(first '("apple" "banana" "coconut"))
(rest '("apple" "banana" "coconut"))
(first (rest '("apple" "banana" "coconut")))
(rest (rest '("apple" "banana" "coconut")))
]

Plait also provides @racket[second], @racket[third], @racket[fourth],
and @racket[list-ref]. Those functions are sometimes useful to extract
pieces of a list that has a known shape. Functions that take the
@racket[first] of a list and recur with the @racket[rest] turn out to
be be more common. Here's a function that check whether
@racket["milk"] is in a list of strings:

@interaction[
(define (got-milk? [_items : (Listof String)])
  (cond
    [(empty? _items) #f]
    [(cons? _items) (or (string=? (first _items) "milk")
                        (got-milk? (rest _items)))]))
(got-milk? empty)
(got-milk? '("milk" "cookies"))
(got-milk? '("cookies" "milk"))
(got-milk? '("cookies" "cream"))
]

@; ----------------------------------------
@section[#:tag "definitions-tutorial"]{Definitions}

The @racket[define] form defines an identifier to be a synonym for a
value:

@interaction[
(define pi 3.14)
pi
(define tau (+ pi pi))
tau
]

The @racket[define] form can also define a function. The difference is
that @racket[define] for a function definition is followed by an open
parenthesis, then the function name, a name for each argument, and a
closing parenthesis. The expression afterward is the body of the
function, which can refer to the function arguments and is evaluated
when the function is called.

@interaction[
(define (circle-area _r)
  (* pi (* _r _r)))
(circle-area 10)
]

Since @secref["getting-started"], we have been evaluating forms only
in DrRacket's bottom area, which is also known as the
@defterm{interactions area}. Definitions normally go in the top
area---which is known as the @defterm{definitions area}, naturally.

Put these two definitions in the definitions area:

@interaction[#:hidden (define (is-even? x) #true)]

@interaction[
#:no-prompt
(define (is-odd? _x)
  (if (zero? _x)
      #f
      (is-even? (- _x 1))))
(code:line
 code:blank
 (define (is-even? _x)
   (if (zero? _x)
       #t
       (is-odd? (- _x 1)))))
]

Click @onscreen{Run}. The functions @racket[is-odd?] and @racket[is-even?]
are now available in the interactions area:

@interaction[
is-odd?
(is-odd? 12)
]

In our definitions of @racket[pi] and @racket[tau], plait inferred
that the newly defined names have type @racket[Number] and that
@racket[is-odd?] has type @racket[(Number -> Boolean)]. Programs are
often easier to read and understand if you write explicitly the type
that would otherwise be inferred. Declaring types can sometimes help
improve or localize error messages when Plait's attempt to infer a
type fails, since inference can other end up depending on the whole
program.

Declare a type for a constant by writing @racket[:] followed by a type
after the defined identifier:

@interaction[
#:no-prompt
(define groceries : (Listof String) '("milk" "cookies"))
]

For a function, attach a type to an argument by writing square
brackets around the argument name, @racket[:], and a type. Write the
function's result type with @racket[:] and the type after the
parentheses that group the function name with its arguments.

@interaction[
#:no-prompt
(define (starts-milk? [_items : (Listof String)]) : Boolean
  (equal? (first _items) "milk"))
]

You can declare local functions and constants by using the
@racket[local] form as a wrapper. The definitions that appear after
@racket[local] are visible only within the @racket[local] form, and
the result of the @racket[local] form is the value of the expression
that appears after the definitions. The definitions must be grouped
with square brackets.

@interaction[
(local [(define pi-ish 3)
        (define (approx-circle-area _r)
          (* pi-ish (* _r _r)))]
   (approx-circle-area 2))
(eval:error (code:line pi-ish (code:comment @#,t{not visible outside the @racket[local]})))
(eval:error (code:line approx-circle-area (code:comment @#,t{not visible outside the @racket[local]})))
]

The @racket[local] form is most often used inside a function to define
a helper function or to avoid a repeated computating involving the
function arguments.

@interaction[
(define (discard-first-if-fruit _items)
  (local [(define _a (first _items))]
    (cond
     [(equal? _a "apple") (rest _items)]
     [(equal? _a "banana") (rest _items)]
     [else _items])))
(discard-first-if-fruit '("apple" "potato"))
(discard-first-if-fruit '("banana" "potato"))
(discard-first-if-fruit '("potato" "apple"))
]

The @racket[let] and @racket[letrec] forms are similar to
@racket[local], but they are somewhat more compact by avoiding the
requirement to write @racket[define]. The
@racket[discard-first-if-fruit] example above can be equivalently
written using @racket[let]:

@interaction[
#:no-prompt
(define (discard-first-if-fruit _items)
  (let ([_a (first _items)])
    (cond
     [(equal? _a "apple") (rest _items)]
     [(equal? _a "banana") (rest _items)]
     [else _items])))
]

@; ----------------------------------------
@section[#:tag "datatypes-tutorial"]{Datatypes}

So far, we have only seen built-in types like @racket[Number] and
@racket[(Listof String)]. Sometimes, it's useful to define your own
name as a shorthand for a type, such as defining @racket[Groceries] to
be equivalent to @racket[(Listof String)]:

@interaction[
#:no-prompt
(define-type-alias Groceries (Listof String))
(define shopping-list : Groceries '("milk" "cookies"))
]

Note that, by convention, all type names are capitalized. Plait is
case-sensitive.

But what if the data that you need to represent is not easily encoded
in existing types, such as when you need to keep track of a tiger's
color and stripe count (which doesn't work as a list, since a list
can't have a number and a string)? And what if your type, say
@racket[Animal], has values of different shapes: tigers that have color
and stripe counts, plus snakes that have a color, weight, and favorite
food?

The @racket[define-type] form handles those generalizations. The
general form is

@racketblock[
(define-type _Type
  (_variant-name_1 [_field-name_1 : _Type_1]
                   [_field-name_2 : _Type_2]
                   ...)
  (_variant-name_2 [_field-name_3 : _Type_3]
                   [_field-name_4 : _Type_4]
                   ...)
  ...)
]

with any number of @defterm{variants} and where each variant has any
number of typed @defterm{fields}. If you're used to Java-style
classes, you can think of @racket[_Type] as an interface, and each
variant is a class that implements the interface. Unlike Java classes,
a variant name doesn't work as a type name; it only works to create
an instance of the variant.

For example, the following definition is suitable for representing
animals that can be either tigers or snakes:

@interaction[
#:no-prompt
(define-type Animal
  (tiger [color : Symbol]
         [stripe-count : Number])
  (snake [color : Symbol]
         [weight : Number]
         [food : String]))
]

After this definition, @racket[Animal] can be used as a type, while
@racket[tiger] and @racket[snake] work as functions to create
@racket[Animal]s:

@interaction[
(tiger 'orange 12)
(snake 'green 10 "rats")
]

The definition of @racket[Animal] creates several additional
functions:

@itemlist[

 @item{@racket[tiger?], which takes an @racket[Animal] and determines
       whether it was created by @racket[tiger] (as opposed to
       @racket[snake]);}

 @item{@racket[snake?], which takes an @racket[Animal] and determines
       whether it was created by @racket[snake] (as opposed to
       @racket[tiger]);}

 @item{@racket[tiger-color] and @racket[tiger-stripe-count], which take
       a tiger @racket[Animal] and extract its color and stripe count,
       respectively; and}

 @item{@racket[snake-color], @racket[snake-weight], and
       @racket[snake-food], which take a snake @racket[Animal] and
       extract its color, weight, and favorite food, respectively.}
       
]

The name @racket[tiger?] was formed by adding a @litchar{?} to the end
of the variant name @racket[tiger], @racket[tiger-color] is formed by
adding a @litchar{-} between the variant name and field name, and so
on.

@interaction[
(define tony (tiger 'orange 12))
(define slimey (snake 'green 10 "rats"))
(tiger? tony)
(tiger? slimey)
(tiger-color tony)
(snake-food slimey)
(eval:error (tiger-color slimey))
]

Note that the type of @racket[(tiger-color slimey)] printed before an
error was reported. That's because @racket[(tiger-color slimey)] is
well-typed as far as Plait can tell, since @racket[tiger-color] wants
an @racket[Animal] and @racket[slimey] has type @racket[Animal]. We'll
see that @racket[type-case] provides an alterntive to selectors like
@racket[tiger-color] that is less dangerous than the selector.

Using @racket[Animal] as a type and the @racket[tiger?] and
@racket[snake?] predicates, we can write a function that extracts the
color of any animal:

@interaction[
(define (animal-color [_a : Animal]) : Symbol
  (cond
    [(tiger? _a) (tiger-color _a)]
    [(snake? _a) (snake-color _a)]))
(animal-color tony)
(animal-color slimey)
]

When writing @racket[animal-color], what if we forget the
@racket[snake?] case? What if we get @racket[snake-color] and
@racket[tiger-color] backwards? Unfortunately, the type checker cannot
help us detect those problems. If we use @racket[type-case], however,
the type checker can help more.

The general form of a @racket[type-case] expresison is

@racketblock[
(type-case _Type _value-expression
  [(_variant-name_1 _field-var_1 _field-var_2 ...)
   _result-expression_1]
  [(_variant-name_2 _field-var_3 _field-var_4 ...)
   _result-expression_2]
  ...)
]

The @racket[_value-expression] must produce a value matching
@racket[_Type]. Every variant of @racket[_Type] must be represented by
a clause with a matching @racket[_variant-name]. For that clause, the
number of @racket[_field-var]s must match the declared number of
fields for the variant. The type checker can check all of those
requirements.

To produce a value, @racket[type-case] determines the variant that is
instanited by the result of @racket[_value-expression]. For the clause
matching that variant (by name), @racket[type-case] makes each
@racket[_field-var] stand for the corresponding field (by position)
within the value, and then evaluates the corresponding
@racket[_result-expression]. Here's @racket[animal-color] rewritten
with @racket[type-case]:

@interaction[
(define (animal-color [_a : Animal]) : Symbol
  (type-case Animal _a
    [(tiger _col _sc) _col]
    [(snake _col _wgt _f) _col]))
(animal-color tony)
(animal-color slimey)
]

Put the definitions of @racket[Anmal] and @racket[animal-color] in
DrRacket's definitions area. Then, you can mouse over @racket[_a] in
@racket[animal-color] to confirm that it means the @racket[_a] that is
passed as an argument. Mouse over @racket[_col] to see that it means
one of the variant-specific fields. Try changing the body of
@racket[animal-color] to leave out a clause or a field variable and
see what error is reported when you hit @onscreen{Run}.

You should think of @racket[type-case] as a pattern-matching form. It
matches a value like @racket[(tiger 'orange 12)] to the pattern
@racket[(tiger _col _sc)] so that @racket[_col] stands for
@racket['orange] and @racket[_sc] stands for @racket[12]. A value like
@racket[(snake 'green 10 "rats")] does not match the pattern
@racket[(tiger _col _sc)], but it matches the pattern
@racket[(snake _col _wgt _f)].

At the end of @secref["lists-tutorial"], we saw a @racket[got-milk?]
function that uses @racket[cond], similar to the way the dangerous
version of @racket[animal-color] uses @racket[cond]. The
@racket[type-case] form works on list types with @racket[empty] and
@racket[(cons _fst _rst)] patterns, so here's an improved
@racket[got-milk?]:

@interaction[
(define (got-milk? [_items : (Listof String)])
  (type-case (Listof String) _items
    [empty #f]
    [(cons _item _rst-items) (or (string=? _item "milk")
                                 (got-milk? _rst-items))]))
(got-milk? empty)
(got-milk? '("cookies" "milk"))
]

Note that there are no parentheses around @racket[empty] in
@racket[got-milk?]. That's because @racket[empty] is never called as a
constructor function---it's simply a constant value---so the pattern
form doesn't have parentheses, either. The @racket[empty] pattern is a
special case in @racket[type-case]; all other variant names in a
@racket[type-case] form will have parentheses, since they will always
be used a constrcutor functions, even if the variant has no fields.

@interaction[
(define-type Grade
  (letter [alpha : Symbol])
  (pass-fail [pass? : Boolean])
  (incomplete))
(letter 'A)
(pass-fail #t)
(incomplete)
(define (passed-course? [_g : Grade]) : Boolean
  (type-case Grade _g
    [(letter _a) (not (eq? _a 'F))]
    [(pass-fail _p?) _p?]
    [(incomplete) #f]))
(passed-course? (letter 'B))
(passed-course? (incomplete))
]

You can also use @racket[else] for a final clause in
@racket[type-case] to catch any variants that are not already covered.

@interaction[
(define (high-pass? [_g : Grade]) : Boolean
  (type-case Grade _g
    [(letter _a) (eq? _a 'A)]
    [else #f]))
(high-pass? (letter 'A))
(high-pass? (incomplete))
]

When you use @racket[else], however, the type checker is less helpful
for making sure that you've considered all cases.


@; ----------------------------------------
@section[#:tag "testing-tutorial"]{Testing and Debugging}

Plait includes built-in support for testing your programs. The
@racket[test] form takes two expressions and makes sure that they
produce the same value. Typically, the first expression is a function
call, and the second expression is the expected result of the
function. The @racket[test] form prints output that starts ``good'' if
the test passes or ``bad'' if it fails.

@interaction[
(define (taste s)
  (cond
    [(equal? s "milk") 'good]
    [else 'not-as-good]))
(test (taste "milk") 'good)
(test (taste "brussel sprouts") 'not-as-good)
(test (taste "beets") 'bad)
]

They say that no news is good news. To suppress the output for passing
tests, so that only failing test strigger output, use
@racket[(print-only-errors #t)].

@interaction[
(print-only-errors #t)
(test (taste "kale") 'not-as-good)
(test (taste "anchovies") 'bad)
]

To test that an expression reports an expected error, use
@racket[test/exn]. The @racket[test/exn] form's section expression
should produce a string, and @racket[test/exn] checks that an error is
reported where the string occurs in the error message. You can only
test for errors that your program specifically reports using the
@racket[error] function.

@interaction[
(define (always-fail [n : Number]) : Number
  (error 'always-fail "we're not actually returning a number"))
(test/exn (always-fail 42) "not actually")
(test/exn (always-fail 42) "should not get called")
]

When you write a program (in the definitions area of DrRacket), the
order of function definitions generally does not matter, even if the
functions call each other. A test at the top level of a program,
however, must appear after all functions that the test may end up
calling. To relax this constraint, wrap tests in a @racket[(module+
test ....)] form. A @racket[(module+ test ....)] wrapper effectively
moves its content to the end of the program.

@racketblock[
(module+ test
  (test (retaste "milk") '(still good)))
code:blank
(define (retaste s)
  (list 'still (taste s)))
]

A good set of tests will cause all expressions in a program to be
evaluated at least once. DrRacket can help you check that your program
has good test coverage. In DrRacket's @onscreen{Language} menu, select
@onscreen{Choose Language}, click @onscreen{Show Details}, click
@onscreen{Submodules to run}, and then select the @onscreen{Syntactic
test suite coverage} option. After selecting that option, when you
@onscreen{Run} a program, it will stay its normal color if all is
well. If some expression has not been covered, however, the program
text will go mostly black, and any expression that has not been
evaluated will turn orange with a black background. Resolve the
problem and restore your program text's color by adding more tests.

When you're debugging a program, it may be helpful to see the
arguments that are passed to a particular function and the results
that the function returns. You can enable that kind of tracing for a
function with the @racket[trace] declaration, which must appear after
the function's definitions.

@racketblock[
(define (got-milk? [_items : (Listof String)])
  (type-case (Listof String) _items
    [empty #f]
    [(cons _item _rst-items) (or (string=? _item "milk")
                                 (got-milk? _rst-items))]))
(trace got-milk?)
]

@interaction[
#:hidden
(require (typed-in racket/trace [trace : ('a -> Void)] [untrace : ('a -> Void)]))
(define supressed (trace got-milk?))
]

@interaction[
(got-milk? empty)
(got-milk? '("cookies" "milk"))
]

@interaction[#:hidden (untrace got-milk?)]

As you're devloping a program, sometimes it's useful to run a partial
program where you haven't yet decided on part of the implementation.
The @racket[....] expression (with four dots) can be used in place of
any expression of any type. A program using @racket[....] can compile
and run, but the @racket[....] reports an error if it is reached
during evaluation.

@interaction[
(define (got-milk? [_items : (Listof String)])
  (type-case (Listof String) _items
    [empty #f]
    [(cons _item _rst-items) ....]))
(got-milk? '())
(eval:error (got-milk? '("cheese")))
]

@; ----------------------------------------
@section[#:tag "lambda-tutorial"]{Anonymous Functions}

After we define a function, the name of the function can be used as a
value without calling it. If you just evaluate the function name, then
Plait will print something like @nonbreaking{@racketresultfont{#<procedure>}}.

@interaction[
(define (plus-five _n)
  (+ _n 5))
plus-five
]

More usefully, you might pass the function to another function that
calls it. For example, the @racket[map] function takes a function and a
list, and it applies the function to each element of the list to
produce a new list.

@interaction[
(map plus-five
     '(1 2 3))
]

Sometimes, and especially with @racket[map], you need a one-off
function that doesn't need to be defined for everyone else to see, and
it doesn't even need a name. You can make an @defterm{anonymous function}
by using @racket[lambda]:

@interaction[
(map (lambda (_n)
       (+ _n 6))
     '(1 2 3))
]

The form @racket[(lambda (_n) (+ _n 6))] means ``the function that
takes an argument @racket[_n] and returns @racket[(+ _n 6)].'' You can
evaluate a @racket[lambda] form without passing it anywhere, although
that isn't particularly useful:

@interaction[
(lambda (_n)
  (+ _n 7))
]

Notice that the result has a function type: it's a function that takes
a @racket[Number] and returns a @racket[Number].

An anonymous function created with @racket[lambda] doesn't have to
@emph{stay} anonymous. Since you can use a @racket[lambda] form
anywhere that an expression is allowed, you can use in
@racket[define]:

@interaction[
#:no-prompt
(define plus-eight : (Number -> Number)
  (lambda (_n)
    (+ _n 8)))
]

This definition is completely equivalent to the function-definition
shorthand:

@interaction[
#:no-prompt
(define (plus-eight [_n : Number]) : Number
  (+ _n 8))
]

Another interesting property of @racket[lambda] functions is that,
just like any local function, the body of a @racket[lambda] can see any
surrounding variable binding. For example, the @racket[lambda] body in
the following @racket[add-to-each] function can see the @racket[_m]
that is passed to @racket[add-to-each]:

@interaction[
(define (add-to-each _m _items)
  (map (lambda (_n)
         (+ _n _m))
       _items))
(add-to-each 7 '(1 2 3))
(add-to-each 70 '(1 2 3))
]

@; ----------------------------------------
@section[#:tag "s-exp-tutorial"]{S-Expressions}

If we write @racket[(+ pi pi)], then given our
@seclink["definitions-tutorial"]{earlier definition} of @racket[pi],
the result is as you'd expect:

@interaction[
(+ pi pi)
]

We could add a @litchar{'} to the front of that expression and get a
completely different result---a list of symbols:

@interaction[
'(+ pi pi)
]

If you're studying programming languages and building interpreters,
this looks like a handy coincidence. You can represent an expression
as a list! Unfortunately, this trick does not always work:

@interaction[
(eval:error '(+ 1 2))
]

A list cannot contain a mixture of numbers and symbols, so it cannot
directly represent the expression @racket[(+ 1 2)].

If you've had some experience programming in Java, you might think
that the solution is a list of @tt{Object}s, because anything can be
coerced to and from the type @tt{Object}. That is, we would be able to
mix a symbol as @tt{Object} with two numbers as @tt{Object}s.

Plait doesn't have an @tt{Object} type, but it does have an
@racket[S-Exp] type, which is similar. Even better, the @racket[S-Exp]
type works with a convenient @litchar{'}-like shortcut. The
@racket[S-Exp] shortcut is @litchar{`} (usually pronounced
``backquote'') instead of @litchar{'}:

@interaction[
`(+ 1 2)
]

When an S-expression is list-like, then you can corece the
S-expression to a list using @racket[s-exp->list]. The result is a
list of @racket[S-Exp]s:

@interaction[
(s-exp->list `(+ 1 2))
]

If an S-expression isn't list-like, the coercion fails. Other
coercions include @racket[s-exp->number] and @racket[s-exp->symbol].
You can go the other way with @racket[number->s-exp],
@racket[symbol->s-exp], and @racket[list->s-exp]. Functions like
@racket[s-exp-list?] and @racket[s-exp-number?] report whether an
S-expression is list-like or number-like.

@interaction[
`1
(eval:error (s-exp->list `1))
(s-exp->number `1)
(number->s-exp 1)
(list->s-exp (list (symbol->s-exp '+)
                   (number->s-exp 1)
                   (number->s-exp 2)))
(s-exp-number? `1)
(s-exp-list? `1)
]

The backquote @litchar{`} versus forward quote @litchar{'} distinction
is subtle. A convention to help highlight the difference is to mostly
use curly braces with @litchar{`}. Curly braces are interchangable
with parentheses and square brackets, and Plait won't print results
with curly braces, but the visual cue can still help when reading
programs.

@interaction[
`{+ 1 2}
`{* 3 {+ 4 x}}
]

The S-expression @litchar{`} has an extra feature that the
list-constructing @litchar{'} lacks: a way to escape back to the
evaluated-expression world by using @litchar{,} (i.e., a comma). The
escaped expression must produce a S-expression, and the result
S-expression takes the place of the escape:

@interaction[
`{+ 1 ,(number->s-exp (+ 3 4))}
]

The @litchar[",@"] escape form is similar to @litchar{,}, but
@litchar[",@"] is a @defterm{splicing} escape that expects a list of
S-expressions and inlines the elements into the enclosing list-like
S-expression.

@interaction[
`{+ ,@(list (number->s-exp 1) (number->s-exp (+ 3 4)))}
`{+ ,(list->s-exp (list (number->s-exp 1) (number->s-exp (+ 3 4))))}
]

@; ----------------------------------------
@section[#:tag "s-exp-match-tutorial"]{S-Expression Matching}

Since the @racket[equal?] function works on any kind of value, it can
compare two S-expressions to determine whether they are the same:

@interaction[
(equal? `{+ 1 2} `{+ 1 2})
(equal? `{+ 1 2} `{+ 1 4})
]

Suppose that you don't just want to recognize @racket[`{+ 1 2}], but
you want to recognize any list-like S-expression that has three
elements where the first element is @racket['+]-like and the other two
elements are number-like. That recognition problem is tedious to
implement, due to the many required many checks and coercions.

@interaction[
(define (is-plus-numbers? se)
  (and (s-exp-list? se)
       (let ([l (s-exp->list se)])
         (and (= 3 (length l))
              (let ([a (first l)])
                (and (s-exp-symbol? a)
                     (eq? '+ (s-exp->symbol a))))
              (s-exp-number? (second l))
              (s-exp-number? (third l))))))
(is-plus-numbers? `{+ 1 2})
(is-plus-numbers? `1)
(is-plus-numbers? `{+ 3 y})
(is-plus-numbers? `{{+} 1 2})
]

The @racket[s-exp-match?] function simplifies recognition tasks for
S-expressions. It's like @racket[equal?] on S-expressions, but the
first S-expression can have special symbols that match different
classes of values, instead of matching themselves literally. The
special symbols include @racket[NUMBER], which matchs any number, so
@racket[is-plus-numbers?] is more simply implemented like this:

@interaction[
(define (is-plus-numbers? se)
  (s-exp-match? `{+ NUMBER NUMBER} se))
(is-plus-numbers? `{+ 1 2})
(is-plus-numbers? `{+ 3 y})
]

Other special symbols include @racket[SYMBOL], which matches any
symbol, and @racket[ANY], which matches anything.

@interaction[
(define (single-argument-lambda? se)
  (s-exp-match? `{lambda {SYMBOL} ANY} se))
(single-argument-lambda? `{lambda {x} {+ x 1}})
(single-argument-lambda? `{lambada 0})
]

The symbol @racket[...] is even more special. It causes the preceeding
S-expression to match zero or more times to cover multiple elements in
an enclosing list. For example, @racket[`{SYMBOL ...}] would match a
list-like S-expression that has any number of symbol-like elements.

@interaction[
(define (any-argument-lambda? se)
  (s-exp-match? `{lambda {SYMBOL ...} ANY} se))
(any-argument-lambda? `{lambda {x} {+ x 1}})
(any-argument-lambda? `{lambda {x y z} {+ x 1}})
(any-argument-lambda? `{lambda {} {+ x 1}})
(any-argument-lambda? `{lambada 0})
]

@; ----------------------------------------
@section[#:tag "tuples-tutorial"]{Tuples and Options}

If you want to combine a small number of values in a single value, and
if the values have different types (so that a list doesn't work), you
can use a @defterm{tuple} as an alternative to creating a new datatype
with a single variant.

The @racket[values] form creates a tuple from any number of values.
The type of a tuple reveals the type of every component value in the
tuple, separating the types with @racket[*].

@interaction[
(values 1 "milk" 'apple)
(values '(1 2 3) #f)
]

Using @racket[values], this @racket[consume] function can effectively
return two values each time that it is called:

@interaction[
#:no-prompt
(define (consume [s : String]) : (Symbol * String)
  (cond
   [(equal? s "milk") (values 'drink "Mmm....")]
   [(equal? s "beets") (values 'eat "Ugh....")]
   [else (values 'sleep "Zzz...")]))
]

To extract the component values from a tuple, match the tuple with
names using @racket[define-values].

@interaction[
(consume "milk")
(define-values (action response) (consume "beets"))
action
response
]

The convenience functions @racket[fst] and @racket[snd] can be used in
the special case of a 2-value tuple to extract the first or second
component.

@interaction[
(snd (consume "milk"))
]

Sometimes, instead of always returning multiple values, you'll want a
function that returns either one value or no value. A tuple is no help
for that case, but Plait predefines a helpful datatype called
@racket[Optionof]:

@racketblock[
(define-type (Optionof 'a)
  (none)
  (some [v : 'a]))
]

The @racket['a] in this definition of @racket[Optionof] indicates that
you can return any kind of value in a @racket[some].

@interaction[
(define (get-slogan [_s : String]) : (Optionof String)
  (cond
   [(equal? _s "milk") (some "It does a body good")]
   [else (none)]))
(get-slogan "milk")
(get-slogan "iced tea")
(type-case (Optionof String) (get-slogan "moon pie")
  [(some s) s]
  [(none) "no comment"])
]

@; ----------------------------------------
@section[#:tag "program-tutorial"]{Programs and Modules}

When you write a program using @racket[@#,hash-lang[]
@#,racketmodname[plait]], you are technically defining a
@defterm{module}. A Plait module contains a mixture of expressions and
definitions. The expressions are evaluated in order, and the value of
each expression is printed after the expression is evaluated (unless
the result value has type @racket[Void]). The order of function
definitions doesn't matter, as long as a function definition appears
before any expression that eventually calls the function.

@racketmod[
plait
code:blank
(define (is-odd? _x)
  (if (zero? _x)
      #f
      (is-even? (- _x 1))))
code:blank
(is-odd? 0) (code:comment "ok")
@#,tt{#;}(is-odd? 1) (code:comment @#,t{won't work, because it needs @racket[is-even?]})
code:blank
(define (is-even? _x)
  (if (zero? _x)
      #t
      (is-odd? (- _x 1))))
code:blank
(is-even? 1) (code:comment "ok")
(is-odd? 1) (code:comment "ok")
]

Note the use of @litchar{#;} in the example above. A @litchar{#;}
comments out the entire form that follows it, which is handy for
commenting out a definition of expression, even when the definition or
expression spans multiple lines.

Modules written with the @racket[module] form can be nested in other
modules. A nested module is called a @defterm{submodule}. Plait
programs don't often use submodules that are written with
@racket[module], but the @racket[module+] form is more common. A
@racket[module+] form creates a submodule by merging all
@racket[module+]s that use the same name. A typical use of
@racket[module+] is to move all of a program's tests into a
@racket[test] submodule.

@racketmod[
plait
code:blank
(define (is-odd? _x)
  (if (zero? _x)
      #f
      (is-even? (- _x 1))))
code:blank
(module+ test
  (is-odd? 0)
  (is-odd? 1))
code:blank
(define (is-even? _x)
  (if (zero? _x)
      #t
      (is-odd? (- _x 1))))
code:blank
(module+ test
  (is-even? 1)
  (is-odd? 1))
]

The submodule name @racket[test] is special, because DrRacket
automatically runs a @racket[test] submodule (if one is present) after
running the enclosing module. In the above example, since the
@racket[test] submodule is run after the encloding module that defines
@racket[is-odd?] and @racket[is-even?], the tests can use all of the
functions. Another advantage of putting tests in a @racket[test]
submodule is that you can turn off the tests. In DrRacket's
@onscreen{Language} menu, select @onscreen{Choose Language}, click
@onscreen{Show Details}, click @onscreen{Submodules to run}, and then
uncheck the @onscreen{test} item.

A Plait module's definitions are automatically exported from the
module. You can import the definitions of another module by using the
@racket[require] form, typically with a string that is a relative path
to the module to import.

@racketmod[#:file "math.rkt"
plait
(define pi 3.14)
(define tau (+ pi pi))
]

@racketmod[#:file "circle.rkt"
plait
(require "math.rkt")

(define (circle-area [r : Number]) : Number
  (* pi (* r r)))
]

A submodule created by @racket[module+] automatically imports the
bindings of the enclosing module, which is why @racket[(module+ test
....)] submodules can automatically access definitions for testing. In
contrast, if you write definitions inside @racket[(module+ test
....)], then the definitions can be used for tests in any
@racket[(module+ test ....)], but the enclosing module will not see
the definitions.

@; ----------------------------------------
@section[#:tag "state-tutorial"]{State}

@nested[#:style 'inset]{ @bold{Warning:} If you are using Plait with a
programming-languages course, then the instructor has almost certainly
disallowed the constructs in this chaper for use in your homework
solutions, except as specifically allowed. Don't use @racket[set!],
@racket[begin], boxes, or vectors unless the instructor says that you
can. If you're tempted to use one of those, you're doing it wrong.}

We have so far described @racket[define] as naming constants, but
names bound by @racket[define] are not necessarily constant. The value
associated to the name can be changed using @racket[set!].

@interaction[
(define gravity 6.6e-11)
gravity
(set! gravity 6.5e-11)
gravity
]

The type of a @racket[set!] expression is @racket[Void], meaning that
it doesn't return a useful value, and the useless value doesn't even
print as a result. If you need to change a variable and then return a
value, use @racket[begin] to sequence the operations. The value of a
@racket[begin] form is the value of its last expression.

@interaction[
(define counter 0)
(define (fresh-number!)
  (begin
    (set! counter (add1 counter))
    counter))
(fresh-number!)
(fresh-number!)
]

The @litchar{!} at the end of @racket[fresh-number!] is a convention
to warn readers that calling the function can have a side effect.

Although you can set a variable's value using @racket[set!], you can't
directly pass a variable to another function that changes the
variable's value. A @racket[set!] on a function's argument would
change the argument variable's value, but would have no effect on the
caller's variables. To make a mutable location that can be passed
around, Plait supports @defterm{boxes}. You can think of a box as a
mutable object that has a single field, where @racket[box] creates a
fresh object, @racket[unbox] extracts the object's field, and
@racket[set-box!] changes the object's field.

@interaction[
(define counter1 (box 0))
(define counter2 (box 0))
(define (fresh-number-at! c)
  (begin
    (set-box! c (add1 (unbox c)))
    (unbox c)))
(fresh-number-at! counter1)
(fresh-number-at! counter1)
(fresh-number-at! counter2)
(fresh-number-at! counter1)
]

A @defterm{vector} is a traditional mutable array. Every element of a
vector must have the same type, which can be inferred from the value
that you suply when making a vector to serve as the initial value for
each of the vector's slots. The @racket[vector] function creates a vector,
@racket[vector-ref] accesses a slot value by position, and
@racket[vector-set!] changes a slot value by position.

@interaction[
(define counters (make-vector 2 0))
(define (fresh-number-at! i)
  (begin
    (vector-set! counters i (add1 (vector-ref counters i)))
    (vector-ref counters i)))
(fresh-number-at! 0)
(fresh-number-at! 0)
(fresh-number-at! 1)
(fresh-number-at! 0)
]


@; ----------------------------------------

@close-eval[plait-eval]

