# commonmark

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

- [`pipe_tables`] (pipe tables)
- [`smart`] (smart quotes, dashes, and ellipses)
- [`strikethrough`] (strikethrough)
- [`superscript`] (superscript)
- [`subscript`] (subscript)
- [`math`] (LaTeX math)
- [`emoji`] (emoji)
- [`autolinks`] (autolink bare URLs and email addresses)
- [`footnotes`] (footnotes)
- [`definition_lists`] (definition lists)
- [`fancy_lists`] (fancy ordered list markers (parentheses, alpha, roman)
- [`attributes`] (attributes for all inline and block elements)
- [`raw_attribute`] (special raw block and inline elements in any format)
- [`bracketed_spans`] (spans of inline elements with attributes)
- [`fenced_divs`] (groups of block elements with attributes)
- [`auto_identifiers`] (automatic generation of identifiers for headings)
- [`implicit_heading_references`] (headings implicitly define link references)

[`pipe_tables`]: test/pipe_tables.md
[`smart`]: test/smart.md
[`strikethrough`]: test/strikethrough.md
[`superscript`]: test/superscript.md
[`subscript`]: test/subscript.md
[`math`]: test/math.md
[`emoji`]: test/emoji.md
[`autolinks`]: test/autolinks.md
[`footnotes`]: test/footnotes.md
[`definition_lists`]: test/definition_lists.md
[`fancy_lists`]: test/fancy_lists.md
[`attributes`]: test/attributes.md
[`raw_attribute`]: test/raw_attribute.md
[`bracketed_spans`]: test/bracketed_spans.md
[`fenced_divs`]: test/fenced_divs.md
[`auto_identifiers`]: test/auto_identifiers.md
[`implicit_heading_references`]: test/implicit_heading_references.md



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

## Performance

Here are some benchmarks on real-world commonmark documents.
To get `benchmark.md`, we concatenated a number of READMEs
collected
[here](https://github.com/fitzgen/common-mark-benchmarks/tree/master/github-explore-frontend-js), five times.  The resulting file was 583K.
The [`bench`](http://hackage.haskell.org/package/bench) tool was
used to run the benchmarks.

 | program                   | time (ms) |
 | -------                   | ---------:|
 | cmark                     |   21      |
 | cheapskate                |  167      |
 | commonmark.js             |  228      |
 | **commonmark-hs**         |  485      |
 | pandoc -f commonmark      | 1305      |
 | pandoc -f markdown_strict | 1733      |

It would be good to improve performance.  I'd welcome suggestions about how
to accomplish this.

