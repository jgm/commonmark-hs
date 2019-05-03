# commonmark-core

This package provides the core parsing functionality
for commonmark, without any renderers.

:construction: **Work in progress!**  The interface is in flux, and
the library is not ready for use yet, but comments on the
API and implementation are very much welcome.

The library is **fully commonmark-compliant** and passes the
test suite.  It is designed to be **customizable and easily
extensible.**  To customize the output, create an
AST, or support a new output format, one need only define some
new typeclass instances.  It is also easy to add new syntax
elements or modify existing ones.

**Accurate information about source positions** is available
for all block and inline elements.  Thus the library can be
used to create an accurate syntax highlighter or
an editor with synced live preview.

Finally, the library has been designed for **robust performance
even in pathological cases**. The parser behaves well on
pathological cases that tend to cause stack overflows or
exponential slowdowns in other parsers, with parsing speed that
varies linearly with input length.

The following optional extensions are provided:

- Pipe tables
- Smart quotes, dashes, and ellipses
- Strikethrough
- LaTeX math
- Autolink bare URLs and email addresses
- Footnotes
- Definition lists
- Header attributes

## Notes on the design

The input is a token stream (`[Tok]`), which can be
be produced from a `Text` using `tokenize`.  The `Tok`
elements record source positions, making these easier
to track.

Extensibility is emphasized throughout.  There are two ways in
which one might want to extend a commonmark converter.  First,
one might want to support an alternate output format, or to
change the output for a given format.  Second, one might want
to add new syntactic elements (e.g., definition lists).

To support both kinds of extension, we export the function

```haskell
parseCommonmarkWith :: (Monad m, IsBlock il bl, IsInline il)
                    => SyntaxSpec m il bl -- ^ Defines syntax
                    -> [Tok] -- ^ Tokenized commonmark input
                    -> m (Either ParseError bl)  -- ^ Result or error
```

The parser function takes two arguments:  a `SyntaxSpec` which
defines parsing for the various syntactic elements, and a list
of tokens.  Output is polymorphic:  you can
convert commonmark to any type that is an instance of the
`IsBlock` typeclass.  This gives tremendous flexibility.
Want to produce HTML? You can use the `Html ()` type defined
in `Commonmark.Types` for basic HTML, or `Html SourceRange`
for HTML with source range attributes on every element.

```haskell
GHCI> :set -XOverloadedStrings
GHCI>
GHCI> parseCommonmarkWith defaultSyntaxSpec (tokenize "source" "Hi there") :: IO (Either ParseError (Html ()))
Right <p>Hi there</p>
> parseCommonmarkWith defaultSyntaxSpec (tokenize "source" "Hi there") :: IO (Either ParseError (Html SourceRange))
Right <p data-sourcepos="source@1:1-1:9">Hi there</p>
```

Want to produce a Pandoc AST?  You can use the type
`Cm a Text.Pandoc.Builder.Blocks` defined in `commonmark-pandoc`.

```haskell
GHCI> parseCommonmarkWith defaultSyntaxSpec (tokenize "source" "Hi there") :: Maybe (Either ParseError (Cm () B.Blocks))
Just (Right (Cm {unCm = Many {unMany = fromList [Para [Str "Hi",Space,Str "there"]]}}))
GHCI> parseCommonmarkWith defaultSyntaxSpec (tokenize "source" "Hi there") :: Maybe (Either ParseError (Cm SourceRange B.Blocks))
Just (Right (Cm {unCm = Many {unMany = fromList [Div ("",[],[("data-pos","source@1:1-1:9")]) [Para [Span ("",[],[("data-pos","source@1:1-1:3")]) [Str "Hi"],Span ("",[],[("data-pos","source@1:3-1:4")]) [Space],Span ("",[],[("data-pos","source@1:4-1:9")]) [Str "there"]]]]}}))
```

If you want to support another format (for example, Haddock's `DocH`),
just define typeclass instances of `IsBlock` and `IsInline` for
your type.

Supporting a new syntactic element generally requires (a) adding
a `SyntaxSpec` for it and (b) defining relevant type class
instances for the element.  See the examples in
`Commonmark.Extensions.*`.  Note that `SyntaxSpec` is a Monoid,
so you can specify `myNewSyntaxSpec <> defaultSyntaxSpec`.

