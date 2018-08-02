#lang scribble/manual
@(require (for-label (only-meta-in 0 plait/untyped)
                     (only-in racket/base only-in)))

@title{Untyped with Typed Syntax}

@defmodulelang[plait/untyped]{
The @racketmodname[plait/untyped] language supports the same syntax as
@racketmodname[plait], but it performs no type checking.}

The @racket[define-syntax], @racket[define-syntax-rule],
@racket[module+], and @racket[require] forms from
@racketmodname[plait/untyped] are the bindings from
@racketmodname[racket/base] instead of @racketmodname[plait].

@deftogether[(
@defidform[typed-in]
@defidform[opaque-type-in]
)]{

Forms for @racket[require] that simulate the ones from
@racketmodname[plait] by expanding to uses of @racket[only-in].}
